use super::stubs;
use super::*;

// -----------------------------------------------------------------
// FFI declarations for macOS
// -----------------------------------------------------------------

extern "C" {
    fn mmap(addr: *mut u8, len: usize, prot: i32, flags: i32, fd: i32, offset: i64) -> *mut u8;
    fn munmap(addr: *mut u8, len: usize) -> i32;
    fn pthread_jit_write_protect_np(enabled: i32);
    fn sys_icache_invalidate(addr: *mut u8, size: usize);
}

const PROT_READ: i32 = 0x01;
const PROT_WRITE: i32 = 0x02;
const PROT_EXEC: i32 = 0x04;
const MAP_PRIVATE: i32 = 0x0002;
const MAP_ANON: i32 = 0x1000; // macOS
const MAP_JIT: i32 = 0x0800; // macOS
const MAP_FAILED: *mut u8 = !0usize as *mut u8;

// -----------------------------------------------------------------
// Aarch64 register names (for readability)
// -----------------------------------------------------------------

#[allow(dead_code)]
mod reg {
    // General-purpose registers
    pub const X0: u8 = 0;
    pub const X1: u8 = 1;
    pub const X2: u8 = 2;
    pub const X3: u8 = 3;
    pub const X4: u8 = 4;
    pub const X5: u8 = 5;
    // Scratch registers
    pub const X9: u8 = 9;
    pub const X10: u8 = 10;
    pub const X11: u8 = 11;
    pub const X12: u8 = 12;
    pub const X13: u8 = 13;
    // Callee-saved
    pub const X19: u8 = 19;
    pub const X20: u8 = 20;
    // Frame pointer & link register
    pub const X29: u8 = 29; // FP
    pub const X30: u8 = 30; // LR
    pub const SP: u8 = 31; // stack pointer (encoded as 31 in some insns)

    // FP/SIMD registers (used for f64 operations)
    pub const D0: u8 = 0;
    pub const D1: u8 = 1;
    pub const D2: u8 = 2;
}

// -----------------------------------------------------------------
// JsValue / Vec / Vm layout constants for inline JIT
// -----------------------------------------------------------------

/// JsValue layout constants for `#[repr(C, u8)]` enum.
/// Tag byte at offset 0, f64 payload at offset 8, total size 32.
pub(crate) mod layout {
    /// Size of one JsValue in bytes.
    #[allow(dead_code)]
    pub const JSVALUE_SIZE: usize = 32;
    /// Offset of the discriminant (tag) byte within JsValue.
    pub const JSVALUE_TAG_OFFSET: usize = 0;
    /// Offset of the f64 payload within JsValue::Number.
    pub const JSVALUE_F64_OFFSET: usize = 8;
    /// Offset of the bool payload within JsValue::Boolean.
    /// With #[repr(C, u8)], fields start at offset 8 (after tag + 7 padding bytes).
    pub const JSVALUE_BOOL_OFFSET: usize = 8;

    /// JsValue discriminant values.
    #[allow(dead_code)]
    pub const TAG_UNDEFINED: u8 = 0;
    #[allow(dead_code)]
    pub const TAG_NULL: u8 = 1;
    pub const TAG_BOOLEAN: u8 = 2;
    pub const TAG_NUMBER: u8 = 3;
    pub const TAG_STRING: u8 = 4;
    pub const TAG_SYMBOL: u8 = 5;
    pub const TAG_OBJECT: u8 = 6;

    /// Offsets within Vm for value_stack's internal fields.
    /// These are probed at runtime on first use since Vec layout is not guaranteed.
    use std::sync::OnceLock;

    struct VecLayout {
        /// Offset of value_stack's data pointer relative to Vm base.
        ptr_offset: usize,
        /// Offset of value_stack's length field relative to Vm base.
        len_offset: usize,
        /// Offset of value_stack's capacity field relative to Vm base.
        cap_offset: usize,
    }

    static VEC_LAYOUT: OnceLock<VecLayout> = OnceLock::new();

    fn probe_layout() -> VecLayout {
        // Use a known capacity value to reliably find the cap field.
        // We use a standalone Vec<JsValue> to discover the relative offsets of
        // {ptr, len, cap} within a Vec<JsValue>, then apply to Vm's value_stack.

        // Step 1: Discover Vec layout by looking at a standalone Vec
        let mut probe_vec: Vec<rawjs_runtime::JsValue> = Vec::with_capacity(37); // unique sentinel
        probe_vec.push(rawjs_runtime::JsValue::Number(0.0));
        let vec_base = &probe_vec as *const Vec<rawjs_runtime::JsValue> as *const u8;
        let vec_size = std::mem::size_of::<Vec<rawjs_runtime::JsValue>>();
        let expected_ptr = probe_vec.as_ptr() as usize;

        let mut vec_ptr_delta = None;
        let mut vec_len_delta = None;
        let mut vec_cap_delta = None;

        unsafe {
            for off in (0..vec_size).step_by(8) {
                let val = *(vec_base.add(off) as *const usize);
                if val == expected_ptr {
                    vec_ptr_delta = Some(off);
                }
                if val == 1 && vec_len_delta.is_none() && Some(off) != vec_ptr_delta {
                    vec_len_delta = Some(off);
                }
                if val == 37 {
                    vec_cap_delta = Some(off);
                }
            }
        }

        let ptr_delta = vec_ptr_delta.expect("failed to probe Vec ptr delta");
        let len_delta = vec_len_delta.expect("failed to probe Vec len delta");
        let cap_delta = vec_cap_delta.expect("failed to probe Vec cap delta");

        // Step 2: Find value_stack within Vm by scanning for its ptr
        let mut vm = crate::Vm::new();
        vm.value_stack.push(rawjs_runtime::JsValue::Number(0.0));

        let vm_base = &vm as *const crate::Vm as *const u8;
        let vm_size = std::mem::size_of::<crate::Vm>();
        let vs_ptr = vm.value_stack.as_ptr() as usize;

        let mut vs_base_offset = None;
        unsafe {
            for off in (0..vm_size - 7).step_by(8) {
                let val = *(vm_base.add(off) as *const usize);
                if val == vs_ptr {
                    // Verify: the len field at the expected delta should be 1
                    let candidate_len_off = off + len_delta - ptr_delta;
                    if candidate_len_off + 8 <= vm_size {
                        let len_val = *(vm_base.add(candidate_len_off) as *const usize);
                        if len_val == 1 {
                            vs_base_offset = Some(off - ptr_delta);
                            break;
                        }
                    }
                }
            }
        }

        let base = vs_base_offset.expect("failed to find value_stack in Vm");

        VecLayout {
            ptr_offset: base + ptr_delta,
            len_offset: base + len_delta,
            cap_offset: base + cap_delta,
        }
    }

    pub fn vm_vs_ptr_offset() -> usize {
        VEC_LAYOUT.get_or_init(probe_layout).ptr_offset
    }

    pub fn vm_vs_len_offset() -> usize {
        VEC_LAYOUT.get_or_init(probe_layout).len_offset
    }

    pub fn vm_vs_cap_offset() -> usize {
        VEC_LAYOUT.get_or_init(probe_layout).cap_offset
    }

    // =================================================================
    // Rc layout probing — needed for inline Rc::clone / Rc::drop
    // =================================================================

    /// Layout of Rust's RcBox<T> (internal to Rc).
    /// RcBox { strong: Cell<usize>, weak: Cell<usize>, value: T }
    /// Rc<T> holds a NonNull<RcBox<T>>.
    struct RcLayout {
        /// Offset of strong count within RcBox.
        strong_offset: usize,
        /// For Rc<str> (fat ptr): offset from data_ptr back to RcBox start.
        /// data_ptr = rcbox_ptr + str_data_back_offset
        /// So rcbox_ptr = data_ptr - str_data_back_offset
        str_data_back_offset: usize,
    }

    static RC_LAYOUT: OnceLock<RcLayout> = OnceLock::new();

    fn probe_rc_layout() -> RcLayout {
        use std::rc::Rc;

        // Probe Rc<str> to find strong_offset and data-to-rcbox distance.
        let rc_str: Rc<str> = Rc::from("probe_sentinel");

        // Get the fat pointer components: (data_ptr, len)
        let fat_ptr: (*const str,) = (Rc::as_ref(&rc_str) as *const str,);
        let data_ptr = fat_ptr.0 as *const u8 as usize;

        // Rc internally holds NonNull<RcBox<str>>. The RcBox is:
        //   { strong: Cell<usize>, weak: Cell<usize>, value: str }
        // The data_ptr points to the `value` field (the str data).
        // strong_count should be 1, weak_count should be 1 (Rust convention).

        // Scan backwards from data_ptr to find strong=1 pattern.
        let mut strong_off = None;
        let mut back_offset = None;
        unsafe {
            // The RcBox header is before the data. Scan up to 64 bytes back.
            for back in (0..=64usize).step_by(8) {
                let candidate = data_ptr - back;
                let val = *(candidate as *const usize);
                if val == 1 {
                    // Check if next usize is also 1 (weak count)
                    let next = *((candidate + 8) as *const usize);
                    if next == 1 {
                        strong_off = Some(0usize); // strong at offset 0 from RcBox
                        back_offset = Some(back);
                        break;
                    }
                }
            }
        }

        RcLayout {
            strong_offset: strong_off.expect("failed to probe Rc strong_count offset"),
            str_data_back_offset: back_offset.expect("failed to probe Rc<str> data back offset"),
        }
    }

    /// Offset of strong_count within RcBox (typically 0).
    pub fn rc_strong_offset() -> usize {
        RC_LAYOUT.get_or_init(probe_rc_layout).strong_offset
    }

    /// For Rc<str>: rcbox_ptr = data_ptr - this value.
    pub fn rc_str_data_back_offset() -> usize {
        RC_LAYOUT.get_or_init(probe_rc_layout).str_data_back_offset
    }

    // =================================================================
    // CallStack / CallFrame layout probing
    // =================================================================

    struct FrameLayout {
        /// Offset of call_stack Vec ptr field relative to Vm base.
        cs_ptr_offset: usize,
        /// Offset of call_stack Vec len field relative to Vm base.
        cs_len_offset: usize,
        /// Size of CallFrame struct in bytes.
        callframe_size: usize,
        /// Offset of `locals` Vec ptr field within CallFrame.
        cf_locals_ptr_delta: usize,
        /// Offset of `locals` Vec len field within CallFrame.
        cf_locals_len_delta: usize,
        /// Offset of `this_value` (32-byte JsValue) within CallFrame.
        cf_this_offset: usize,
    }

    static FRAME_LAYOUT: OnceLock<FrameLayout> = OnceLock::new();

    fn probe_frame_layout() -> FrameLayout {
        let callframe_size = std::mem::size_of::<crate::CallFrame>();

        // First, find Vec<JsValue> internal deltas (reuse logic from probe_layout)
        let mut probe_locals: Vec<rawjs_runtime::JsValue> = Vec::with_capacity(41);
        probe_locals.push(rawjs_runtime::JsValue::Number(0.0));
        let locals_base = &probe_locals as *const Vec<rawjs_runtime::JsValue> as *const u8;
        let vec_size = std::mem::size_of::<Vec<rawjs_runtime::JsValue>>();
        let locals_ptr_val = probe_locals.as_ptr() as usize;

        let mut vec_ptr_delta = None;
        let mut vec_len_delta = None;

        unsafe {
            for off in (0..vec_size).step_by(8) {
                let val = *(locals_base.add(off) as *const usize);
                if val == locals_ptr_val && vec_ptr_delta.is_none() {
                    vec_ptr_delta = Some(off);
                }
                if val == 1 && vec_len_delta.is_none() && Some(off) != vec_ptr_delta {
                    vec_len_delta = Some(off);
                }
            }
        }
        let vec_ptr_d = vec_ptr_delta.expect("failed to probe Vec ptr delta (frame)");
        let vec_len_d = vec_len_delta.expect("failed to probe Vec len delta (frame)");

        // Create a CallFrame with known sentinel values to probe offsets.
        let sentinel_this = rawjs_runtime::JsValue::Number(123456789.0);
        let sentinel_this_bits = 123456789.0f64.to_bits();

        let mut locals_vec = Vec::with_capacity(41);
        locals_vec.push(rawjs_runtime::JsValue::Number(0.0));
        let locals_ptr_sentinel = locals_vec.as_ptr() as usize;

        let frame = crate::CallFrame {
            chunk_index: 0xDEAD_BEEF,
            ip: 0xCAFE_BABE,
            base: 0,
            locals: locals_vec,
            arguments: Vec::new(),
            arguments_object: None,
            callee: None,
            is_strict: false,
            upvalues: Vec::new(),
            this_value: sentinel_this,
        };

        let frame_base = &frame as *const crate::CallFrame as *const u8;

        // Find locals Vec ptr within CallFrame
        let mut cf_locals_ptr = None;
        let mut cf_this = None;

        unsafe {
            for off in (0..callframe_size).step_by(8) {
                let val = *(frame_base.add(off) as *const usize);
                if val == locals_ptr_sentinel && cf_locals_ptr.is_none() {
                    // Verify len at expected delta
                    let len_off = off + vec_len_d - vec_ptr_d;
                    if len_off + 8 <= callframe_size {
                        let len_val = *(frame_base.add(len_off) as *const usize);
                        if len_val == 1 {
                            cf_locals_ptr = Some(off);
                        }
                    }
                }
            }

            // Find this_value: look for the sentinel f64 bits
            for off in (0..callframe_size - 7).step_by(8) {
                let val = *(frame_base.add(off) as *const u64);
                if val == sentinel_this_bits {
                    // this_value's f64 is at (this_offset + JSVALUE_F64_OFFSET)
                    // So this_offset = off - 8
                    let this_off = off - JSVALUE_F64_OFFSET;
                    // Verify tag byte = TAG_NUMBER (3)
                    if *(frame_base.add(this_off) as *const u8) == TAG_NUMBER {
                        cf_this = Some(this_off);
                        break;
                    }
                }
            }
        }

        let cf_locals_ptr_off = cf_locals_ptr.expect("failed to find locals Vec in CallFrame");
        let cf_locals_len_off = cf_locals_ptr_off + vec_len_d - vec_ptr_d;

        // Now find call_stack Vec in Vm
        let mut vm = crate::Vm::new();
        // Push a frame with unique chunk_index
        vm.call_stack.push(crate::CallFrame {
            chunk_index: 0xBAAD_F00D,
            ip: 0,
            base: 0,
            locals: Vec::new(),
            arguments: Vec::new(),
            arguments_object: None,
            callee: None,
            is_strict: false,
            upvalues: Vec::new(),
            this_value: rawjs_runtime::JsValue::Undefined,
        });

        let vm_base = &vm as *const crate::Vm as *const u8;
        let vm_size = std::mem::size_of::<crate::Vm>();
        let cs_data_ptr = vm.call_stack.as_ptr() as usize;

        let mut cs_base_offset = None;
        unsafe {
            for off in (0..vm_size - 7).step_by(8) {
                let val = *(vm_base.add(off) as *const usize);
                if val == cs_data_ptr {
                    // Check len at expected delta
                    let len_off = off + vec_len_d - vec_ptr_d;
                    if len_off + 8 <= vm_size {
                        let len_val = *(vm_base.add(len_off) as *const usize);
                        if len_val == 1 {
                            cs_base_offset = Some(off);
                            break;
                        }
                    }
                }
            }
        }

        let cs_ptr_off = cs_base_offset.expect("failed to find call_stack in Vm");
        let cs_len_off = cs_ptr_off + vec_len_d - vec_ptr_d;

        // Prevent drops from corrupting
        std::mem::forget(frame);

        FrameLayout {
            cs_ptr_offset: cs_ptr_off,
            cs_len_offset: cs_len_off,
            callframe_size,
            cf_locals_ptr_delta: cf_locals_ptr_off,
            cf_locals_len_delta: cf_locals_len_off,
            cf_this_offset: cf_this.expect("failed to find this_value in CallFrame"),
        }
    }

    pub fn vm_cs_ptr_offset() -> usize {
        FRAME_LAYOUT.get_or_init(probe_frame_layout).cs_ptr_offset
    }

    pub fn vm_cs_len_offset() -> usize {
        FRAME_LAYOUT.get_or_init(probe_frame_layout).cs_len_offset
    }

    pub fn callframe_size() -> usize {
        FRAME_LAYOUT.get_or_init(probe_frame_layout).callframe_size
    }

    pub fn cf_locals_ptr_delta() -> usize {
        FRAME_LAYOUT
            .get_or_init(probe_frame_layout)
            .cf_locals_ptr_delta
    }

    pub fn cf_locals_len_delta() -> usize {
        FRAME_LAYOUT
            .get_or_init(probe_frame_layout)
            .cf_locals_len_delta
    }

    pub fn cf_this_offset() -> usize {
        FRAME_LAYOUT.get_or_init(probe_frame_layout).cf_this_offset
    }
}

// -----------------------------------------------------------------
// JitCompiler
// -----------------------------------------------------------------

/// Compiles a bytecode `Chunk` to aarch64 machine code using stub calls.
pub struct JitCompiler {
    /// Buffer of emitted machine code bytes.
    code: Vec<u8>,
    /// Map from bytecode instruction index to byte offset in `code`.
    pc_map: Vec<usize>,
    /// Deferred jump patches: (byte offset of the jump instruction,
    /// target bytecode instruction index).
    jump_patches: Vec<(usize, usize)>,
}

impl JitCompiler {
    fn new() -> Self {
        JitCompiler {
            code: Vec::with_capacity(4096),
            pc_map: Vec::new(),
            jump_patches: Vec::new(),
        }
    }

    // =============================================================
    // Public entry point
    // =============================================================

    pub fn compile(chunk: &Chunk) -> Option<JitFunction> {
        if !Self::is_jittable(chunk) {
            return None;
        }

        let mut compiler = JitCompiler::new();
        compiler.emit_prologue();

        for (i, instr) in chunk.instructions.iter().enumerate() {
            compiler.pc_map.push(compiler.code.len());
            compiler.emit_instruction(*instr, i);
        }
        // Sentinel entry so that pc_map[len] is valid for jump patches.
        compiler.pc_map.push(compiler.code.len());

        compiler.patch_jumps();

        // Success epilogue: mov w0, #0; restore; ret
        compiler.emit_success_epilogue();

        Some(compiler.finalize())
    }

    /// Returns false for chunks containing instructions that cannot be JIT-compiled.
    fn is_jittable(chunk: &Chunk) -> bool {
        !chunk.instructions.iter().any(|instr| {
            matches!(
                instr,
                Instruction::New(_)
                    | Instruction::InitGlobal(_)
                    | Instruction::LoadGlobalOrUndefined(_)
                    | Instruction::DeleteName(_)
                    | Instruction::DeleteProperty
                    | Instruction::LoadArguments
            )
        })
    }

    // =============================================================
    // Prologue / epilogue
    // =============================================================

    /// Emit function prologue.
    ///
    /// New ABI: x0 = *mut Vm. We save x29/x30/x19, then mov x19, x0.
    fn emit_prologue(&mut self) {
        // stp x29, x30, [sp, #-16]!
        self.emit_stp_pre(reg::X29, reg::X30, reg::SP, -16);
        // mov x29, sp
        self.emit_mov_reg(reg::X29, reg::SP);

        // Save x19 (callee-saved)
        self.emit_stp_pre(reg::X19, reg::X20, reg::SP, -16);

        // mov x19, x0  (save Vm pointer)
        self.emit_mov_reg(reg::X19, reg::X0);
    }

    /// Emit the success epilogue: w0 = 0, restore, ret.
    fn emit_success_epilogue(&mut self) {
        // mov w0, #0
        self.emit_movz_w(reg::X0, 0);
        self.emit_epilogue_body();
    }

    /// Emit the error epilogue label target: w0 = 1, restore, ret.
    fn emit_error_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        // mov w0, #1
        self.emit_movz_w(reg::X0, 1);
        self.emit_epilogue_body();
        offset
    }

    /// Emit the "frame done" epilogue: w0 already set by stub_return (2),
    /// just restore and ret.
    fn emit_frame_done_epilogue(&mut self) -> usize {
        let offset = self.code.len();
        // w0 already = 0 (success -- Return stub already handled everything)
        self.emit_movz_w(reg::X0, 0);
        self.emit_epilogue_body();
        offset
    }

    /// Shared restore-and-return body.
    fn emit_epilogue_body(&mut self) {
        // Restore x19, x20
        self.emit_ldp_post(reg::X19, reg::X20, reg::SP, 16);
        // ldp x29, x30, [sp], #16
        self.emit_ldp_post(reg::X29, reg::X30, reg::SP, 16);
        // ret
        self.emit_ret();
    }

    // =============================================================
    // Instruction emission
    // =============================================================

    fn emit_instruction(&mut self, instr: Instruction, bc_index: usize) {
        match instr {
            // --- Stack ops & literals ---
            Instruction::LoadConst(idx) => {
                self.emit_stub_call_1(stubs::stub_load_const as *const () as usize, idx as u32);
            }
            Instruction::LoadLocal(idx) => {
                self.emit_inline_load_local(stubs::stub_load_local as *const () as usize, idx);
            }
            Instruction::StoreLocal(idx) => {
                self.emit_inline_store_local(stubs::stub_store_local as *const () as usize, idx);
            }
            Instruction::LoadGlobal(idx) => {
                self.emit_stub_call_1(stubs::stub_load_global as *const () as usize, idx as u32);
            }
            Instruction::InitGlobal(_) => unreachable!(),
            Instruction::LoadGlobalOrUndefined(_) => unreachable!(),
            Instruction::StoreGlobal(idx) => {
                self.emit_stub_call_1(stubs::stub_store_global as *const () as usize, idx as u32);
            }
            Instruction::LoadUpvalue(idx) => {
                self.emit_stub_call_1(stubs::stub_load_upvalue as *const () as usize, idx as u32);
            }
            Instruction::StoreUpvalue(idx) => {
                self.emit_stub_call_1(stubs::stub_store_upvalue as *const () as usize, idx as u32);
            }
            Instruction::Pop => {
                self.emit_inline_pop(stubs::stub_pop as *const () as usize);
            }
            Instruction::Dup => {
                self.emit_inline_dup(stubs::stub_dup as *const () as usize);
            }
            Instruction::Undefined => {
                self.emit_inline_push_literal(
                    stubs::stub_push_undefined as *const () as usize,
                    layout::TAG_UNDEFINED,
                    None,
                );
            }
            Instruction::Null => {
                self.emit_inline_push_literal(
                    stubs::stub_push_null as *const () as usize,
                    layout::TAG_NULL,
                    None,
                );
            }
            Instruction::True => {
                self.emit_inline_push_literal(
                    stubs::stub_push_true as *const () as usize,
                    layout::TAG_BOOLEAN,
                    Some(1),
                );
            }
            Instruction::False => {
                self.emit_inline_push_literal(
                    stubs::stub_push_false as *const () as usize,
                    layout::TAG_BOOLEAN,
                    Some(0),
                );
            }
            Instruction::This => {
                self.emit_inline_this(stubs::stub_push_this as *const () as usize);
            }

            // --- Arithmetic (inline fast path for Number-Number) ---
            Instruction::Add => {
                self.emit_inline_number_binop(
                    stubs::stub_add as *const () as usize,
                    Self::emit_fadd_d,
                );
            }
            Instruction::Sub => {
                self.emit_inline_number_binop(
                    stubs::stub_sub as *const () as usize,
                    Self::emit_fsub_d,
                );
            }
            Instruction::Mul => {
                self.emit_inline_number_binop(
                    stubs::stub_mul as *const () as usize,
                    Self::emit_fmul_d,
                );
            }
            Instruction::Div => {
                self.emit_inline_number_binop(
                    stubs::stub_div as *const () as usize,
                    Self::emit_fdiv_d,
                );
            }
            Instruction::Mod => {
                extern "C" {
                    fn fmod(x: f64, y: f64) -> f64;
                }
                self.emit_inline_libc_binop(
                    stubs::stub_modulo as *const () as usize,
                    fmod as *const () as usize,
                );
            }
            Instruction::Exp => {
                extern "C" {
                    fn pow(x: f64, y: f64) -> f64;
                }
                self.emit_inline_libc_binop(
                    stubs::stub_exp as *const () as usize,
                    pow as *const () as usize,
                );
            }
            Instruction::Neg => {
                self.emit_inline_neg(stubs::stub_neg as *const () as usize);
            }
            Instruction::Pos => {
                self.emit_inline_pos(stubs::stub_pos as *const () as usize);
            }
            Instruction::Not => {
                self.emit_inline_not(stubs::stub_not as *const () as usize);
            }
            Instruction::BitNot => {
                self.emit_inline_bitnot(stubs::stub_bit_not as *const () as usize);
            }
            Instruction::TypeOf => self.emit_stub_call_0(stubs::stub_typeof as *const () as usize),
            Instruction::Void => {
                self.emit_inline_void(stubs::stub_void as *const () as usize);
            }
            Instruction::Delete => {
                self.emit_inline_delete(stubs::stub_delete as *const () as usize);
            }
            Instruction::DeleteName(_) | Instruction::DeleteProperty => unreachable!(),

            // --- Comparison (inline fast path for Number-Number) ---
            Instruction::Eq => self.emit_stub_call_0(stubs::stub_eq as *const () as usize),
            Instruction::StrictEq => {
                self.emit_inline_strict_eq(stubs::stub_strict_eq as *const () as usize, false);
            }
            Instruction::Ne => self.emit_stub_call_0(stubs::stub_ne as *const () as usize),
            Instruction::StrictNe => {
                self.emit_inline_strict_eq(stubs::stub_strict_ne as *const () as usize, true);
            }
            Instruction::Lt => {
                self.emit_inline_number_cmp(stubs::stub_lt as *const () as usize, Self::COND_MI);
            }
            Instruction::Le => {
                self.emit_inline_number_cmp(stubs::stub_le as *const () as usize, Self::COND_LS);
            }
            Instruction::Gt => {
                self.emit_inline_number_cmp(stubs::stub_gt as *const () as usize, Self::COND_GT);
            }
            Instruction::Ge => {
                self.emit_inline_number_cmp(stubs::stub_ge as *const () as usize, Self::COND_GE);
            }

            // --- Bitwise (inline fast path for Number-Number) ---
            Instruction::BitAnd => {
                self.emit_inline_bitwise_binop(
                    stubs::stub_bit_and as *const () as usize,
                    Self::emit_and_reg_w,
                );
            }
            Instruction::BitOr => {
                self.emit_inline_bitwise_binop(
                    stubs::stub_bit_or as *const () as usize,
                    Self::emit_orr_reg_w,
                );
            }
            Instruction::BitXor => {
                self.emit_inline_bitwise_binop(
                    stubs::stub_bit_xor as *const () as usize,
                    Self::emit_eor_reg_w,
                );
            }
            Instruction::Shl => {
                self.emit_inline_shift(
                    stubs::stub_shl as *const () as usize,
                    Self::emit_lslv_w,
                    false,
                );
            }
            Instruction::Shr => {
                self.emit_inline_shift(
                    stubs::stub_shr as *const () as usize,
                    Self::emit_asrv_w,
                    false,
                );
            }
            Instruction::UShr => {
                self.emit_inline_shift(
                    stubs::stub_ushr as *const () as usize,
                    Self::emit_lsrv_w,
                    true,
                );
            }

            // --- Relational ---
            Instruction::In => self.emit_stub_call_0(stubs::stub_in as *const () as usize),
            Instruction::Instanceof => {
                self.emit_stub_call_0(stubs::stub_instanceof as *const () as usize)
            }
            Instruction::LoadArguments => unreachable!(),

            // --- Postfix (inline fast path for Number) ---
            Instruction::PostfixIncrement => {
                self.emit_inline_postfix(stubs::stub_postfix_inc as *const () as usize, true);
            }
            Instruction::PostfixDecrement => {
                self.emit_inline_postfix(stubs::stub_postfix_dec as *const () as usize, false);
            }

            // --- Object / Array ---
            Instruction::CreateObject => {
                self.emit_stub_call_0(stubs::stub_create_object as *const () as usize)
            }
            Instruction::CreateArray(count) => {
                self.emit_stub_call_1(stubs::stub_create_array as *const () as usize, count as u32);
            }
            Instruction::GetProperty(idx) => {
                self.emit_stub_call_1(stubs::stub_get_property as *const () as usize, idx as u32);
            }
            Instruction::SetProperty(idx) => {
                self.emit_stub_call_1(stubs::stub_set_property as *const () as usize, idx as u32);
            }
            Instruction::GetIndex => {
                self.emit_stub_call_0(stubs::stub_get_index as *const () as usize)
            }
            Instruction::SetIndex => {
                self.emit_stub_call_0(stubs::stub_set_index as *const () as usize)
            }
            Instruction::GetComputed => {
                self.emit_stub_call_0(stubs::stub_get_computed as *const () as usize)
            }
            Instruction::SetComputed => {
                self.emit_stub_call_0(stubs::stub_set_computed as *const () as usize)
            }
            Instruction::CreateClosure(idx) => {
                self.emit_stub_call_1(stubs::stub_create_closure as *const () as usize, idx as u32);
            }

            // --- Function calls ---
            Instruction::Call(argc) => {
                self.emit_stub_call_1(stubs::stub_call as *const () as usize, argc as u32);
            }
            Instruction::New(_) => {
                unreachable!("JIT does not support NEW");
            }
            Instruction::CallMethod(argc) => {
                self.emit_stub_call_1(stubs::stub_call_method as *const () as usize, argc as u32);
            }
            Instruction::Return => {
                // Call stub_return which returns 2 for "frame done"
                self.emit_stub_call_0_return(stubs::stub_return as *const () as usize);
            }

            // --- Control flow (jumps) ---
            Instruction::Jump(offset) => {
                let target = ((bc_index as i64) + 1 + (offset as i64)) as usize;
                let patch_offset = self.code.len();
                // B #0 (placeholder)
                self.emit_u32(0x14000000);
                self.jump_patches.push((patch_offset, target));
            }
            Instruction::JumpIfFalse(offset) => {
                // Call stub_pop_and_test_truthy: returns 0=falsy, 1=truthy
                // mov x0, x19
                self.emit_mov_reg(reg::X0, reg::X19);
                // Load stub address and call
                self.emit_load_and_call(stubs::stub_pop_and_test_truthy as *const () as usize);
                // cbz w0, <target>  (if falsy, jump)
                let target = ((bc_index as i64) + 1 + (offset as i64)) as usize;
                let patch_offset = self.code.len();
                self.emit_u32(0x34000000); // CBZ w0, #0 (placeholder)
                self.jump_patches.push((patch_offset, target));
            }
            Instruction::JumpIfTrue(offset) => {
                // Call stub_pop_and_test_truthy: returns 0=falsy, 1=truthy
                self.emit_mov_reg(reg::X0, reg::X19);
                self.emit_load_and_call(stubs::stub_pop_and_test_truthy as *const () as usize);
                // cbnz w0, <target>  (if truthy, jump)
                let target = ((bc_index as i64) + 1 + (offset as i64)) as usize;
                let patch_offset = self.code.len();
                self.emit_u32(0x35000000); // CBNZ w0, #0 (placeholder)
                self.jump_patches.push((patch_offset, target));
            }

            // --- Iterator ---
            Instruction::GetIterator => {
                self.emit_stub_call_0(stubs::stub_get_iterator as *const () as usize);
            }
            Instruction::IteratorNext => {
                self.emit_stub_call_0(stubs::stub_iterator_next as *const () as usize);
            }
            Instruction::IteratorDone(offset) => {
                // stub_iterator_done_check(vm, offset) -> 0=not done, 1=done, 0x80000000=error
                // We pass the offset so the stub can adjust IP for the interpreter's
                // internal state, but the JIT also handles the jump in machine code.
                self.emit_mov_reg(reg::X0, reg::X19);
                self.emit_movz_w(reg::X1, 0); // offset not used for JIT jump, stub adjusts IP
                                              // Actually we need to load signed offset into w1
                self.emit_load_i32(reg::X1, offset);
                self.emit_load_and_call(stubs::stub_iterator_done_check as *const () as usize);
                // Check for error (bit 31 set)
                // tst w0, #0x80000000
                self.emit_tst_imm(reg::X0, 0x80000000);
                // b.ne error_exit (placeholder — will be patched at finalize)
                let error_patch = self.code.len();
                self.emit_u32(0x54000001); // B.NE #0 placeholder
                                           // TODO: error_exit patch — for now store this
                self.jump_patches.push((error_patch, usize::MAX)); // sentinel for error exit

                // Check if done: cmp w0, #1
                self.emit_cmp_imm(reg::X0, 1);
                // b.eq <target>
                let target = ((bc_index as i64) + 1 + (offset as i64)) as usize;
                let patch_offset = self.code.len();
                self.emit_u32(0x54000000); // B.EQ #0 placeholder
                self.jump_patches.push((patch_offset, target));
            }

            // --- For-in ---
            Instruction::ForInInit => {
                self.emit_stub_call_0(stubs::stub_for_in_init as *const () as usize);
            }
            Instruction::ForInNext(offset) => {
                self.emit_mov_reg(reg::X0, reg::X19);
                self.emit_load_i32(reg::X1, offset);
                self.emit_load_and_call(stubs::stub_for_in_next as *const () as usize);
                // Check for error
                self.emit_tst_imm(reg::X0, 0x80000000);
                let error_patch = self.code.len();
                self.emit_u32(0x54000001); // B.NE error
                self.jump_patches.push((error_patch, usize::MAX));
                // Check if done
                self.emit_cmp_imm(reg::X0, 1);
                let target = ((bc_index as i64) + 1 + (offset as i64)) as usize;
                let patch_offset = self.code.len();
                self.emit_u32(0x54000000); // B.EQ <target>
                self.jump_patches.push((patch_offset, target));
            }

            // --- ESM ---
            Instruction::ImportModule(idx) => {
                self.emit_stub_call_1(stubs::stub_import_module as *const () as usize, idx as u32);
            }
            Instruction::ImportBinding(idx) => {
                self.emit_stub_call_1(stubs::stub_import_binding as *const () as usize, idx as u32);
            }
            Instruction::ExportBinding(idx) => {
                self.emit_stub_call_1(stubs::stub_export_binding as *const () as usize, idx as u32);
            }
            Instruction::ExportDefault => {
                self.emit_stub_call_0(stubs::stub_export_default as *const () as usize);
            }

            // --- Exception handling ---
            Instruction::Throw => {
                self.emit_stub_call_0(stubs::stub_throw as *const () as usize);
            }
            Instruction::EnterTry(catch_offset, finally_offset) => {
                self.emit_stub_call_2i(
                    stubs::stub_enter_try as *const () as usize,
                    catch_offset,
                    finally_offset,
                );
            }
            Instruction::LeaveTry => {
                self.emit_stub_call_0(stubs::stub_leave_try as *const () as usize);
            }

            // --- Generator / async / dispose ---
            Instruction::CreateGenerator => {
                self.emit_stub_call_0(stubs::stub_create_generator as *const () as usize);
            }
            Instruction::Yield => {
                // Yield pops the frame — treat like Return (return 2 = frame done)
                self.emit_stub_call_0_return(stubs::stub_yield as *const () as usize);
            }
            Instruction::Await => {
                // Await suspends the frame — treat like Return (return 2 = frame done)
                self.emit_stub_call_0_return(stubs::stub_await as *const () as usize);
            }
            Instruction::DisposeResource(slot) => {
                self.emit_stub_call_1(
                    stubs::stub_dispose_resource as *const () as usize,
                    slot as u32,
                );
            }
        }
    }

    // =============================================================
    // Stub call emission patterns
    // =============================================================

    /// Emit: mov x0, x19; load stub addr; blr x9; cbnz w0, error_exit
    /// For stubs with no extra operand: fn(vm) -> u32
    fn emit_stub_call_0(&mut self, stub_addr: usize) {
        // mov x0, x19
        self.emit_mov_reg(reg::X0, reg::X19);
        // Load stub address into x9 and blr x9
        self.emit_load_and_call(stub_addr);
        // cbnz w0, error_exit (placeholder)
        self.emit_error_check();
    }

    /// Emit: mov x0, x19; mov w1, #operand; load stub addr; blr x9; cbnz w0, error_exit
    /// For stubs with one u32 operand: fn(vm, u32) -> u32
    fn emit_stub_call_1(&mut self, stub_addr: usize, operand: u32) {
        // mov x0, x19
        self.emit_mov_reg(reg::X0, reg::X19);
        // mov w1, #operand
        self.emit_mov_imm32(reg::X1, operand);
        // Load stub address into x9 and blr x9
        self.emit_load_and_call(stub_addr);
        // cbnz w0, error_exit (placeholder)
        self.emit_error_check();
    }

    /// Emit: mov x0, x19; load w1, #op1; load w2, #op2; call stub; cbnz w0, error_exit
    /// For stubs with two i32 operands: fn(vm, i32, i32) -> u32
    fn emit_stub_call_2i(&mut self, stub_addr: usize, op1: i32, op2: i32) {
        // mov x0, x19
        self.emit_mov_reg(reg::X0, reg::X19);
        // load w1, #op1
        self.emit_load_i32(reg::X1, op1);
        // load w2, #op2
        self.emit_load_i32(reg::X2, op2);
        // Load stub address into x9 and blr x9
        self.emit_load_and_call(stub_addr);
        // cbnz w0, error_exit (placeholder)
        self.emit_error_check();
    }

    /// Emit stub call for Return instruction.
    /// stub_return returns 2 for "frame done". We check:
    ///   cmp w0, #2 → b.eq frame_done_epilogue
    ///   cbnz w0, error_exit
    fn emit_stub_call_0_return(&mut self, stub_addr: usize) {
        // mov x0, x19
        self.emit_mov_reg(reg::X0, reg::X19);
        // Load stub address and call
        self.emit_load_and_call(stub_addr);
        // cmp w0, #2
        self.emit_cmp_imm(reg::X0, 2);
        // b.eq frame_done (placeholder — will be patched to success epilogue)
        let frame_done_patch = self.code.len();
        self.emit_u32(0x54000000); // B.EQ #0
        self.jump_patches.push((frame_done_patch, usize::MAX - 1)); // sentinel for frame_done

        // cbnz w0, error_exit
        self.emit_error_check();
    }

    /// Emit cbnz w0, error_exit_placeholder
    fn emit_error_check(&mut self) {
        let patch_offset = self.code.len();
        self.emit_u32(0x35000000); // CBNZ w0, #0 (placeholder)
        self.jump_patches.push((patch_offset, usize::MAX)); // sentinel for error exit
    }

    // =============================================================
    // Jump patching
    // =============================================================

    fn patch_jumps(&mut self) {
        // Emit error epilogue at the end
        let error_exit_offset = self.emit_error_epilogue();
        let frame_done_offset = self.emit_frame_done_epilogue();

        for &(code_offset, target_bc) in &self.jump_patches {
            let target_code_offset = if target_bc == usize::MAX {
                // Error exit sentinel
                error_exit_offset
            } else if target_bc == usize::MAX - 1 {
                // Frame done sentinel
                frame_done_offset
            } else if target_bc < self.pc_map.len() {
                self.pc_map[target_bc]
            } else {
                // Target past end — point to success epilogue area.
                self.code.len()
            };

            let delta = (target_code_offset as i64 - code_offset as i64) / 4;

            let existing = u32::from_le_bytes([
                self.code[code_offset],
                self.code[code_offset + 1],
                self.code[code_offset + 2],
                self.code[code_offset + 3],
            ]);

            let patched = if existing & 0xFC000000 == 0x14000000 {
                // Unconditional branch B: imm26
                let imm26 = (delta as u32) & 0x03FFFFFF;
                0x14000000 | imm26
            } else if existing & 0xFF000010 == 0x54000000 {
                // Conditional branch B.cond: imm19 at bits [23:5], cond at [3:0]
                let cond = existing & 0xF;
                let imm19 = ((delta as u32) & 0x7FFFF) << 5;
                0x54000000 | imm19 | cond
            } else if existing & 0xFF000000 == 0x34000000 {
                // CBZ: imm19 at bits [23:5], Rt at [4:0]
                let rt = existing & 0x1F;
                let imm19 = ((delta as u32) & 0x7FFFF) << 5;
                0x34000000 | imm19 | rt
            } else if existing & 0xFF000000 == 0x35000000 {
                // CBNZ: imm19 at bits [23:5], Rt at [4:0]
                let rt = existing & 0x1F;
                let imm19 = ((delta as u32) & 0x7FFFF) << 5;
                0x35000000 | imm19 | rt
            } else {
                existing
            };

            let bytes = patched.to_le_bytes();
            self.code[code_offset] = bytes[0];
            self.code[code_offset + 1] = bytes[1];
            self.code[code_offset + 2] = bytes[2];
            self.code[code_offset + 3] = bytes[3];
        }
    }

    // =============================================================
    // Finalize: allocate executable memory, copy code, flush cache
    // =============================================================

    fn finalize(self) -> JitFunction {
        let size = self.code.len();
        let page_size = 16384usize;
        let alloc_size = (size + page_size - 1) & !(page_size - 1);

        let ptr = unsafe {
            let p = mmap(
                std::ptr::null_mut(),
                alloc_size,
                PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON | MAP_JIT,
                -1,
                0,
            );
            assert_ne!(p, MAP_FAILED, "mmap failed for JIT code");
            p
        };

        unsafe {
            pthread_jit_write_protect_np(0);
            std::ptr::copy_nonoverlapping(self.code.as_ptr(), ptr, size);
            pthread_jit_write_protect_np(1);
            sys_icache_invalidate(ptr, size);
        }

        JitFunction {
            code: ptr,
            size: alloc_size,
        }
    }

    // =============================================================
    // aarch64 instruction encoding helpers
    // =============================================================

    #[inline]
    fn emit_u32(&mut self, inst: u32) {
        self.code.extend_from_slice(&inst.to_le_bytes());
    }

    /// `stp Xt1, Xt2, [Xn, #imm]!` -- store pair, pre-index.
    fn emit_stp_pre(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let inst: u32 = 0xA9800000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ldp Xt1, Xt2, [Xn], #imm` -- load pair, post-index.
    fn emit_ldp_post(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let inst: u32 = 0xA8C00000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `mov Xd, Xn` -- encoded as `orr Xd, xzr, Xn`.
    fn emit_mov_reg(&mut self, rd: u8, rn: u8) {
        let inst: u32 = 0xAA000000
            | ((rn as u32 & 0x1F) << 16)
            | (0x1F << 5) // Rm = XZR
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ret`
    fn emit_ret(&mut self) {
        self.emit_u32(0xD65F03C0);
    }

    /// `movz Wd, #imm16` (32-bit register)
    fn emit_movz_w(&mut self, xd: u8, imm16: u16) {
        let inst: u32 = 0x52800000 | ((imm16 as u32) << 5) | (xd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `movz Xd, #imm16, LSL #(hw*16)`
    fn emit_movz(&mut self, xd: u8, imm16: u16, hw: u8) {
        let inst: u32 =
            0xD2800000 | ((hw as u32 & 0x3) << 21) | ((imm16 as u32) << 5) | (xd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `movk Xd, #imm16, LSL #(hw*16)`
    fn emit_movk(&mut self, xd: u8, imm16: u16, hw: u8) {
        let inst: u32 =
            0xF2800000 | ((hw as u32 & 0x3) << 21) | ((imm16 as u32) << 5) | (xd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// Load a 64-bit immediate into an X register using MOVZ + MOVK.
    fn emit_mov_imm64(&mut self, xd: u8, imm: u64) {
        let hw0 = (imm & 0xFFFF) as u32;
        let hw1 = ((imm >> 16) & 0xFFFF) as u32;
        let hw2 = ((imm >> 32) & 0xFFFF) as u32;
        let hw3 = ((imm >> 48) & 0xFFFF) as u32;

        let halfwords = [hw0, hw1, hw2, hw3];
        let first_nonzero = halfwords.iter().position(|&h| h != 0);

        match first_nonzero {
            None => {
                self.emit_movz(xd, 0, 0);
            }
            Some(first) => {
                self.emit_movz(xd, halfwords[first] as u16, first as u8);
                for (i, &hw) in halfwords.iter().enumerate().skip(first + 1) {
                    if hw != 0 {
                        self.emit_movk(xd, hw as u16, i as u8);
                    }
                }
            }
        }
    }

    /// Load a 32-bit immediate into Wd using MOVZ + MOVK.
    fn emit_mov_imm32(&mut self, xd: u8, imm: u32) {
        let lo = (imm & 0xFFFF) as u16;
        let hi = ((imm >> 16) & 0xFFFF) as u16;
        // movz Wd, #lo
        let inst: u32 = 0x52800000 | ((lo as u32) << 5) | (xd as u32 & 0x1F);
        self.emit_u32(inst);
        if hi != 0 {
            // movk Wd, #hi, LSL #16
            let inst: u32 = 0x72A00000 | ((hi as u32) << 5) | (xd as u32 & 0x1F);
            self.emit_u32(inst);
        }
    }

    /// Load a signed i32 into Wd (may need movn for negatives).
    fn emit_load_i32(&mut self, xd: u8, val: i32) {
        if val >= 0 {
            self.emit_mov_imm32(xd, val as u32);
        } else {
            // Use movn for negative values: MOVN Wd, #(~val & 0xFFFF)
            let nval = (!val) as u32;
            let lo = (nval & 0xFFFF) as u16;
            let hi = ((nval >> 16) & 0xFFFF) as u16;
            // movn Wd, #lo
            let inst: u32 = 0x12800000 | ((lo as u32) << 5) | (xd as u32 & 0x1F);
            self.emit_u32(inst);
            if hi != 0 {
                // movk Wd, #((val as u32 >> 16) & 0xFFFF), LSL #16
                let actual_hi = ((val as u32) >> 16) & 0xFFFF;
                let inst: u32 = 0x72A00000 | ((actual_hi) << 5) | (xd as u32 & 0x1F);
                self.emit_u32(inst);
            }
        }
    }

    /// Load a 64-bit absolute address into x9 and `blr x9`.
    fn emit_load_and_call(&mut self, addr: usize) {
        self.emit_mov_imm64(reg::X9, addr as u64);
        // blr x9
        let inst: u32 = 0xD63F0000 | ((reg::X9 as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    /// `cmp Wn, #imm12` (32-bit compare with immediate)
    fn emit_cmp_imm(&mut self, rn: u8, imm12: u32) {
        // SUBS WZR, Wn, #imm12
        let inst: u32 = 0x7100001F | ((imm12 & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    /// `tst Wn, #imm` — AND with immediate, setting flags.
    /// For simplicity, we encode `tst w0, #0x80000000` specifically.
    fn emit_tst_imm(&mut self, rn: u8, imm: u32) {
        if imm == 0x80000000 {
            // TST W0, #0x80000000
            // ANDS WZR, Wn, #0x80000000
            // Logical immediate encoding for 0x80000000: N=0, immr=1, imms=0
            // 32-bit: 0x72000000 | (immr << 16) | (imms << 10) | (Rn << 5) | Rd
            // immr=1, imms=0 for 0x80000000
            let inst: u32 = 0x7200001F | (1 << 16) | ((rn as u32 & 0x1F) << 5);
            self.emit_u32(inst);
        }
        // For other values, we'd need a general logical immediate encoder.
        // Not needed currently.
    }

    // =============================================================
    // aarch64 FP & memory instruction encoding helpers
    // =============================================================

    /// `ldr Xt, [Xn, #imm]` -- 64-bit load, unsigned offset.
    /// imm must be 8-byte aligned and fit in 12 bits after scaling.
    fn emit_ldr_x(&mut self, rt: u8, rn: u8, imm: u32) {
        let scaled = imm / 8;
        let inst: u32 =
            0xF9400000 | ((scaled & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `str Xt, [Xn, #imm]` -- 64-bit store, unsigned offset.
    fn emit_str_x(&mut self, rt: u8, rn: u8, imm: u32) {
        let scaled = imm / 8;
        let inst: u32 =
            0xF9000000 | ((scaled & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ldrb Wt, [Xn, #imm]` -- 8-bit load, unsigned offset.
    fn emit_ldrb(&mut self, rt: u8, rn: u8, imm: u32) {
        let inst: u32 =
            0x39400000 | ((imm & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `strb Wt, [Xn, #imm]` -- 8-bit store, unsigned offset.
    fn emit_strb(&mut self, rt: u8, rn: u8, imm: u32) {
        let inst: u32 =
            0x39000000 | ((imm & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ldr Dt, [Xn, #imm]` -- 64-bit FP load, unsigned offset (imm 8-byte aligned).
    fn emit_ldr_d(&mut self, dt: u8, rn: u8, imm: u32) {
        let scaled = imm / 8;
        let inst: u32 =
            0xFD400000 | ((scaled & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (dt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `str Dt, [Xn, #imm]` -- 64-bit FP store, unsigned offset (imm 8-byte aligned).
    fn emit_str_d(&mut self, dt: u8, rn: u8, imm: u32) {
        let scaled = imm / 8;
        let inst: u32 =
            0xFD000000 | ((scaled & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (dt as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fsub Dd, Dn, Dm` -- double-precision subtract.
    fn emit_fsub_d(&mut self, rd: u8, rn: u8, rm: u8) {
        let inst: u32 = 0x1E603800
            | ((rm as u32 & 0x1F) << 16)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fmul Dd, Dn, Dm` -- double-precision multiply.
    fn emit_fmul_d(&mut self, rd: u8, rn: u8, rm: u8) {
        let inst: u32 = 0x1E600800
            | ((rm as u32 & 0x1F) << 16)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fdiv Dd, Dn, Dm` -- double-precision divide.
    fn emit_fdiv_d(&mut self, rd: u8, rn: u8, rm: u8) {
        let inst: u32 = 0x1E601800
            | ((rm as u32 & 0x1F) << 16)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fneg Dd, Dn` -- double-precision negate.
    fn emit_fneg_d(&mut self, rd: u8, rn: u8) {
        let inst: u32 = 0x1E614000 | ((rn as u32 & 0x1F) << 5) | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fcmp Dn, Dm` -- double-precision compare, setting NZCV.
    fn emit_fcmp_d(&mut self, rn: u8, rm: u8) {
        let inst: u32 = 0x1E602000 | ((rm as u32 & 0x1F) << 16) | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    /// `cset Wd, <cond>` -- set Wd to 1 if condition is true, else 0.
    /// Encoded as `csinc Wd, WZR, WZR, <inverted_cond>`.
    fn emit_cset_w(&mut self, rd: u8, cond: u8) {
        // Invert the condition (flip bit 0)
        let inv_cond = cond ^ 1;
        let inst: u32 = 0x1A9F07E0 | ((inv_cond as u32 & 0xF) << 12) | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `sub Xd, Xn, #imm12` -- 64-bit subtract immediate.
    fn emit_sub_imm_x(&mut self, rd: u8, rn: u8, imm12: u32) {
        let inst: u32 =
            0xD1000000 | ((imm12 & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `add Xd, Xn, #imm12` -- 64-bit add immediate.
    fn emit_add_imm_x(&mut self, rd: u8, rn: u8, imm12: u32) {
        let inst: u32 =
            0x91000000 | ((imm12 & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `add Xd, Xn, Xm, LSL #shift` -- 64-bit add with shift.
    fn emit_add_shifted_x(&mut self, rd: u8, rn: u8, rm: u8, shift: u8) {
        let inst: u32 = 0x8B000000
            | ((rm as u32 & 0x1F) << 16)
            | ((shift as u32 & 0x3F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `lsl Xd, Xn, #shift` -- logical shift left immediate (64-bit).
    /// Encoded as `ubfm Xd, Xn, #(64-shift), #(63-shift)`.
    fn emit_lsl_imm_x(&mut self, rd: u8, rn: u8, shift: u8) {
        let immr = (64 - shift as u32) & 0x3F;
        let imms = (63 - shift as u32) & 0x3F;
        let inst: u32 = 0xD3400000
            | ((immr & 0x3F) << 16)
            | ((imms & 0x3F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `subs Xd, Xn, #imm12` -- 64-bit subtract immediate, setting flags.
    #[allow(dead_code)]
    fn emit_subs_imm_x(&mut self, rd: u8, rn: u8, imm12: u32) {
        let inst: u32 =
            0xF1000000 | ((imm12 & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5) | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `b.cond #offset` -- conditional branch (offset in bytes, will be encoded as imm19).
    #[allow(dead_code)]
    fn emit_bcond(&mut self, cond: u8, byte_offset: i32) {
        let imm19 = ((byte_offset / 4) as u32) & 0x7FFFF;
        let inst: u32 = 0x54000000 | (imm19 << 5) | (cond as u32 & 0xF);
        self.emit_u32(inst);
    }

    /// `b #offset` -- unconditional branch (offset in bytes).
    #[allow(dead_code)]
    fn emit_b(&mut self, byte_offset: i32) {
        let imm26 = ((byte_offset / 4) as u32) & 0x03FFFFFF;
        let inst: u32 = 0x14000000 | imm26;
        self.emit_u32(inst);
    }

    /// `cmp Wn, Wm` -- 32-bit register compare.
    fn emit_cmp_reg_w(&mut self, rn: u8, rm: u8) {
        // SUBS WZR, Wn, Wm (shifted register, no shift)
        let inst: u32 = 0x6B00001F | ((rm as u32 & 0x1F) << 16) | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    /// `cmp Xn, #imm12` -- 64-bit compare with immediate.
    fn emit_cmp_imm_x(&mut self, rn: u8, imm12: u32) {
        // SUBS XZR, Xn, #imm12
        let inst: u32 = 0xF100001F | ((imm12 & 0xFFF) << 10) | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    // =============================================================
    // aarch64 condition codes
    // =============================================================

    #[allow(dead_code)]
    const COND_EQ: u8 = 0x0;
    #[allow(dead_code)]
    const COND_NE: u8 = 0x1;
    #[allow(dead_code)]
    const COND_LO: u8 = 0x3; // unsigned lower (aka CC)
    const COND_MI: u8 = 0x4; // minus/negative — used for FP less-than (NaN-safe)
    const COND_LS: u8 = 0x9; // unsigned lower or same — used for FP less-or-equal (NaN-safe)
    #[allow(dead_code)]
    const COND_GE: u8 = 0xA;
    const COND_GT: u8 = 0xC;
    #[allow(dead_code)]
    const COND_LE: u8 = 0xD;

    // =============================================================
    // Inline Number fast-path emission
    // =============================================================

    /// Emit inline Number-Number binary arithmetic operation (sub/mul/div/mod).
    ///
    /// Fast path:
    /// 1. Load value_stack.ptr and value_stack.len from Vm
    /// 2. Check len >= 2
    /// 3. Load tags of top-2 values, check both are Number (tag == 3)
    /// 4. Load f64 payloads into D0, D1
    /// 5. Perform FP operation
    /// 6. Write result (Number) in place of second-to-top, decrement len by 1
    ///
    /// Slow path: fall through to stub call.
    ///
    /// `fp_op` is a closure that emits the actual FP instruction (rd=D0, rn=D0, rm=D1).
    fn emit_inline_number_binop(&mut self, stub_addr: usize, fp_op: fn(&mut Self, u8, u8, u8)) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();

        // x11 = value_stack.ptr (pointer to data buffer)
        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        // x12 = value_stack.len
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        // x10 = address of value_stack.len field (for writing back)
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);

        // Check len >= 2
        self.emit_cmp_imm_x(reg::X12, 2);
        // b.lo slow_path (unsigned lower = len < 2)
        let slow_branch1 = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32); // B.LO placeholder

        // Compute addresses of top-2 JsValues:
        //   top   = ptr + (len-1) * 32
        //   below = ptr + (len-2) * 32
        // x0 = len - 1
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        // x1 = len - 2
        self.emit_sub_imm_x(reg::X1, reg::X12, 2);
        // x0 = x0 * 32 = x0 << 5
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        // x1 = x1 * 32 = x1 << 5
        self.emit_lsl_imm_x(reg::X1, reg::X1, 5);
        // x0 = ptr + offset_top  (address of rhs JsValue)
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
        // x1 = ptr + offset_below (address of lhs JsValue)
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Load tags: ldrb w2, [x0, #TAG_OFFSET]; ldrb w3, [x1, #TAG_OFFSET]
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        // Check rhs tag == TAG_NUMBER (3)
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch2 = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE placeholder

        // Check lhs tag == TAG_NUMBER (3)
        self.emit_cmp_imm(reg::X3, layout::TAG_NUMBER as u32);
        let slow_branch3 = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE placeholder

        // --- Fast path: both are Number ---
        // Load f64 payloads
        // d1 = rhs.f64 (ldr d1, [x0, #F64_OFFSET])
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        // d0 = lhs.f64 (ldr d0, [x1, #F64_OFFSET])
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);

        // Perform FP operation: d0 = d0 op d1
        fp_op(self, reg::D0, reg::D0, reg::D1);

        // Write result back to lhs slot (x1):
        // - tag stays TAG_NUMBER (already 3) -- no need to write
        // - str d0, [x1, #F64_OFFSET]
        self.emit_str_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);

        // Decrement len: value_stack.len = len - 1
        self.emit_sub_imm_x(reg::X12, reg::X12, 1);
        self.emit_str_x(reg::X12, reg::X10, 0);

        // Skip slow path
        let skip_slow = self.code.len();
        self.emit_u32(0x14000000); // B placeholder

        // --- Slow path ---
        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        // Patch slow-path branches
        self.patch_local_branch(slow_branch1, slow_path);
        self.patch_local_branch(slow_branch2, slow_path);
        self.patch_local_branch(slow_branch3, slow_path);
        // Patch skip-slow branch
        self.patch_local_branch(skip_slow, end);
    }

    /// Emit inline comparison (Lt/Le/Gt/Ge) for Number and Boolean types.
    ///
    /// Fast paths:
    ///   - Number vs Number → fcmp with specified condition
    ///   - Boolean vs Boolean → integer compare of bool bytes (0/1)
    ///
    /// The same condition code works for both FP and {0,1} integer comparison.
    /// Falls to stub for all other type combinations (String, mixed types, etc).
    fn emit_inline_number_cmp(&mut self, stub_addr: usize, cond: u8) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();

        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);

        // Check len >= 2
        self.emit_cmp_imm_x(reg::X12, 2);
        let stub_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // Compute addresses of top-2 JsValues
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        self.emit_sub_imm_x(reg::X1, reg::X12, 2);
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        self.emit_lsl_imm_x(reg::X1, reg::X1, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Load tags
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        // Check rhs tag == TAG_NUMBER
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let check_bool_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE → check_bool

        // Check lhs tag == TAG_NUMBER
        self.emit_cmp_imm(reg::X3, layout::TAG_NUMBER as u32);
        let stub_branch_lhs_num = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE → stub

        // --- Number-Number path ---
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_fcmp_d(reg::D0, reg::D1);
        self.emit_cset_w(reg::X2, cond);
        let number_to_write = self.code.len();
        self.emit_u32(0x14000000); // B → write_result

        // --- Check Boolean ---
        let check_bool = self.code.len();
        self.emit_cmp_imm(reg::X2, layout::TAG_BOOLEAN as u32);
        let stub_branch_rhs_bool = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE → stub

        self.emit_cmp_imm(reg::X3, layout::TAG_BOOLEAN as u32);
        let stub_branch_lhs_bool = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE → stub

        // --- Boolean-Boolean path: compare bool bytes as integers ---
        self.emit_ldrb(reg::X4, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_ldrb(reg::X5, reg::X1, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_cmp_reg_w(reg::X5, reg::X4); // cmp lhs, rhs
        self.emit_cset_w(reg::X2, cond);
        // Fall through to write_result

        // --- Shared write_result ---
        let write_result = self.code.len();
        self.emit_movz(reg::X3, 0, 0);
        self.emit_str_x(reg::X3, reg::X1, 0);
        self.emit_str_x(reg::X3, reg::X1, 8);
        self.emit_str_x(reg::X3, reg::X1, 16);
        self.emit_str_x(reg::X3, reg::X1, 24);
        self.emit_movz_w(reg::X3, layout::TAG_BOOLEAN as u16);
        self.emit_strb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_strb(reg::X2, reg::X1, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_sub_imm_x(reg::X12, reg::X12, 1);
        self.emit_str_x(reg::X12, reg::X10, 0);
        let skip_stub = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // --- Stub path ---
        let stub_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        // Patch branches
        self.patch_local_branch(stub_branch_len, stub_path);
        self.patch_local_branch(check_bool_branch, check_bool);
        self.patch_local_branch(stub_branch_lhs_num, stub_path);
        self.patch_local_branch(number_to_write, write_result);
        self.patch_local_branch(stub_branch_rhs_bool, stub_path);
        self.patch_local_branch(stub_branch_lhs_bool, stub_path);
        self.patch_local_branch(skip_stub, end);
    }

    /// Emit inline unary Neg for Number values.
    fn emit_inline_neg(&mut self, stub_addr: usize) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();

        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch1 = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // x0 = ptr + (len-1) * 32
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);

        // Check tag == NUMBER
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch2 = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // Load f64, negate, store back
        self.emit_ldr_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_fneg_d(reg::D0, reg::D0);
        self.emit_str_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        // Tag stays NUMBER, len unchanged

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch1, slow_path);
        self.patch_local_branch(slow_branch2, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    /// Emit inline StrictEq / StrictNe for all primitive types.
    ///
    /// Handles inline:
    ///   - Different types → false (===) / true (!==)
    ///   - Number === Number → fcmp
    ///   - Boolean === Boolean → byte compare
    ///   - Undefined === Undefined → true
    ///   - Null === Null → true
    ///
    /// Falls to stub only for String/Symbol/Object same-type comparisons.
    fn emit_inline_strict_eq(&mut self, stub_addr: usize, negate: bool) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();

        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);

        // Check len >= 2
        self.emit_cmp_imm_x(reg::X12, 2);
        let stub_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // Compute addresses of top-2 JsValues
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        self.emit_sub_imm_x(reg::X1, reg::X12, 2);
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        self.emit_lsl_imm_x(reg::X1, reg::X1, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Load tags
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        // Compare tags: if different → result is false (===) / true (!==)
        self.emit_cmp_reg_w(reg::X2, reg::X3);
        let diff_tags_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32); // B.NE → different_tags

        // Same type: dispatch by tag
        // Check Number (3)
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let number_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → number_path

        // Check Boolean (2)
        self.emit_cmp_imm(reg::X2, layout::TAG_BOOLEAN as u32);
        let boolean_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → boolean_path

        // Check Undefined (0) or Null (1): tag <= 1
        self.emit_cmp_imm(reg::X2, 1);
        let undef_null_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS → undef_null_path

        // Remaining (String/Symbol/Object) → stub
        let to_stub_branch = self.code.len();
        self.emit_u32(0x14000000); // B → stub

        // --- Number path: fcmp ---
        let number_path = self.code.len();
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_fcmp_d(reg::D0, reg::D1);
        if negate {
            self.emit_cset_w(reg::X2, Self::COND_NE);
        } else {
            self.emit_cset_w(reg::X2, Self::COND_EQ);
        }
        let number_to_write = self.code.len();
        self.emit_u32(0x14000000); // B → write_result

        // --- Boolean path: byte compare ---
        let boolean_path = self.code.len();
        self.emit_ldrb(reg::X4, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_ldrb(reg::X5, reg::X1, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_cmp_reg_w(reg::X5, reg::X4);
        if negate {
            self.emit_cset_w(reg::X2, Self::COND_NE);
        } else {
            self.emit_cset_w(reg::X2, Self::COND_EQ);
        }
        let boolean_to_write = self.code.len();
        self.emit_u32(0x14000000); // B → write_result

        // --- Undefined/Null path: same type always equal ---
        let undef_null_path = self.code.len();
        if negate {
            self.emit_movz_w(reg::X2, 0); // undefined !== undefined → false
        } else {
            self.emit_movz_w(reg::X2, 1); // undefined === undefined → true
        }
        let undef_null_to_write = self.code.len();
        self.emit_u32(0x14000000); // B → write_result

        // --- Different tags: always not equal ---
        let different_tags_path = self.code.len();
        if negate {
            self.emit_movz_w(reg::X2, 1); // x !== y → true
        } else {
            self.emit_movz_w(reg::X2, 0); // x === y → false
        }
        // Fall through to write_result

        // --- Shared write_result: zero JsValue, write Boolean, decrement len ---
        let write_result = self.code.len();
        self.emit_movz(reg::X3, 0, 0);
        self.emit_str_x(reg::X3, reg::X1, 0);
        self.emit_str_x(reg::X3, reg::X1, 8);
        self.emit_str_x(reg::X3, reg::X1, 16);
        self.emit_str_x(reg::X3, reg::X1, 24);
        self.emit_movz_w(reg::X3, layout::TAG_BOOLEAN as u16);
        self.emit_strb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_strb(reg::X2, reg::X1, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_sub_imm_x(reg::X12, reg::X12, 1);
        self.emit_str_x(reg::X12, reg::X10, 0);
        let skip_stub = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // --- Stub path (String/Symbol/Object or len < 2) ---
        let stub_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        // Patch all branches
        self.patch_local_branch(stub_branch_len, stub_path);
        self.patch_local_branch(diff_tags_branch, different_tags_path);
        self.patch_local_branch(number_branch, number_path);
        self.patch_local_branch(boolean_branch, boolean_path);
        self.patch_local_branch(undef_null_branch, undef_null_path);
        self.patch_local_branch(to_stub_branch, stub_path);
        self.patch_local_branch(number_to_write, write_result);
        self.patch_local_branch(boolean_to_write, write_result);
        self.patch_local_branch(undef_null_to_write, write_result);
        self.patch_local_branch(skip_stub, end);
    }

    // =============================================================
    // aarch64 additional instruction encoding helpers
    // =============================================================

    /// `fadd Dd, Dn, Dm` -- double-precision add.
    fn emit_fadd_d(&mut self, rd: u8, rn: u8, rm: u8) {
        let inst: u32 = 0x1E602800
            | ((rm as u32 & 0x1F) << 16)
            | ((rn as u32 & 0x1F) << 5)
            | (rd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fcvtzs Wd, Dn` -- convert f64 to signed i32 (truncate toward zero).
    fn emit_fcvtzs_d_w(&mut self, wd: u8, dn: u8) {
        // FCVTZS Wd, Dn: 0x1E780000
        let inst: u32 = 0x1E780000 | ((dn as u32 & 0x1F) << 5) | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `scvtf Dd, Wn` -- convert signed i32 to f64.
    fn emit_scvtf_d_w(&mut self, dd: u8, wn: u8) {
        // SCVTF Dd, Wn: 0x1E620000
        let inst: u32 = 0x1E620000 | ((wn as u32 & 0x1F) << 5) | (dd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ucvtf Dd, Wn` -- convert unsigned i32 to f64.
    fn emit_ucvtf_d_w(&mut self, dd: u8, wn: u8) {
        // UCVTF Dd, Wn: 0x1E630000
        let inst: u32 = 0x1E630000 | ((wn as u32 & 0x1F) << 5) | (dd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `and Wd, Wn, Wm` -- 32-bit bitwise AND (register).
    fn emit_and_reg_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x0A000000
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `orr Wd, Wn, Wm` -- 32-bit bitwise OR (register).
    fn emit_orr_reg_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x2A000000
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `eor Wd, Wn, Wm` -- 32-bit bitwise XOR (register).
    fn emit_eor_reg_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x4A000000
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `mvn Wd, Wn` -- bitwise NOT (= ORN Wd, WZR, Wn).
    fn emit_mvn_w(&mut self, wd: u8, wn: u8) {
        let inst: u32 = 0x2A2003E0 | ((wn as u32 & 0x1F) << 16) | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `lslv Wd, Wn, Wm` -- logical shift left variable (32-bit).
    fn emit_lslv_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x1AC02000
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `asrv Wd, Wn, Wm` -- arithmetic shift right variable (32-bit).
    fn emit_asrv_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x1AC02800
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `lsrv Wd, Wn, Wm` -- logical shift right variable (32-bit).
    fn emit_lsrv_w(&mut self, wd: u8, wn: u8, wm: u8) {
        let inst: u32 = 0x1AC02400
            | ((wm as u32 & 0x1F) << 16)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `and Wd, Wn, #imm` -- 32-bit AND with logical immediate.
    /// Only supports #0x1F (shift mask for 5-bit values).
    fn emit_and_imm_w(&mut self, wd: u8, wn: u8, imm: u32) {
        // For 0x1F: N=0, immr=0, imms=0b000100 (4)
        // Logical immediate encoding for 32-bit: 0x12000000
        let (immr, imms) = if imm == 0x1F {
            (0u32, 4u32) // encodes 0x1F
        } else {
            panic!("emit_and_imm_w: unsupported immediate 0x{:x}", imm);
        };
        let inst: u32 = 0x12000000
            | ((immr & 0x3F) << 16)
            | ((imms & 0x3F) << 10)
            | ((wn as u32 & 0x1F) << 5)
            | (wd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `stp Xt1, Xt2, [Xn, #imm]` -- store pair, signed offset (not pre/post-indexed).
    fn emit_stp_offset(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let inst: u32 = 0xA9000000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `ldp Xt1, Xt2, [Xn, #imm]` -- load pair, signed offset.
    fn emit_ldp_offset(&mut self, rt1: u8, rt2: u8, rn: u8, imm: i16) {
        let imm7 = ((imm / 8) as u32) & 0x7F;
        let inst: u32 = 0xA9400000
            | (imm7 << 15)
            | ((rt2 as u32 & 0x1F) << 10)
            | ((rn as u32 & 0x1F) << 5)
            | (rt1 as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `fcmp Dn, #0.0` -- compare f64 register against zero.
    fn emit_fcmp_d_zero(&mut self, rn: u8) {
        // FCMP Dn, #0.0: 0x1E602008 | (Rn << 5)
        let inst: u32 = 0x1E602008 | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    /// `eor Wd, Wn, #imm` -- 32-bit XOR with immediate.
    /// Only supports #1 (flip bit 0).
    fn emit_eor_imm_w(&mut self, wd: u8, wn: u8, imm: u32) {
        if imm == 1 {
            // N=0, immr=0, imms=0 → encodes #1
            // EOR Wd, Wn, #1: N=0, immr=0, imms=0
            let inst: u32 = 0x52000000 | ((wn as u32 & 0x1F) << 5) | (wd as u32 & 0x1F);
            self.emit_u32(inst);
        } else {
            panic!("emit_eor_imm_w: unsupported immediate {}", imm);
        }
    }

    // =============================================================
    // Common helpers for inline fast paths
    // =============================================================

    /// Load value_stack state from Vm:
    /// - x11 = value_stack.ptr (data buffer pointer)
    /// - x12 = value_stack.len
    /// - x10 = address of value_stack.len field (for write-back)
    /// - x13 = value_stack.cap
    fn emit_load_vs_state(&mut self) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();
        let vs_cap_off = layout::vm_vs_cap_offset();

        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);
        self.emit_ldr_x(reg::X13, reg::X19, vs_cap_off as u32);
    }

    /// Load value_stack state without cap:
    /// - x11 = value_stack.ptr
    /// - x12 = value_stack.len
    /// - x10 = address of value_stack.len field
    fn emit_load_vs_state_no_cap(&mut self) {
        let vs_ptr_off = layout::vm_vs_ptr_offset();
        let vs_len_off = layout::vm_vs_len_offset();

        self.emit_ldr_x(reg::X11, reg::X19, vs_ptr_off as u32);
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);
    }

    /// Compute TOS address into x0: x0 = ptr + (len-1) * 32.
    /// Assumes x11=ptr, x12=len already loaded.
    fn emit_compute_tos_addr(&mut self) {
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
    }

    /// Compute TOS and TOS-1 addresses: x0=TOS addr, x1=TOS-1 addr.
    /// Assumes x11=ptr, x12=len already loaded.
    fn emit_compute_tos2_addrs(&mut self) {
        self.emit_sub_imm_x(reg::X0, reg::X12, 1);
        self.emit_sub_imm_x(reg::X1, reg::X12, 2);
        self.emit_lsl_imm_x(reg::X0, reg::X0, 5);
        self.emit_lsl_imm_x(reg::X1, reg::X1, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);
    }

    /// Zero a 32-byte JsValue at address in `base_reg`.
    /// Uses stp xzr, xzr for two 16-byte writes.
    fn emit_zero_jsvalue(&mut self, base_reg: u8) {
        // XZR is encoded as register 31
        self.emit_stp_offset(31, 31, base_reg, 0);
        self.emit_stp_offset(31, 31, base_reg, 16);
    }

    /// Write a Number JsValue at address in `base_reg` with f64 in `fp_reg`.
    /// Assumes the slot has already been zeroed.
    fn emit_write_number(&mut self, base_reg: u8, fp_reg: u8) {
        self.emit_movz_w(reg::X2, layout::TAG_NUMBER as u16);
        self.emit_strb(reg::X2, base_reg, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_str_d(fp_reg, base_reg, layout::JSVALUE_F64_OFFSET as u32);
    }

    /// Write a Boolean JsValue at address in `base_reg` with bool in `bool_reg` (w).
    /// Assumes the slot has already been zeroed.
    /// Write a Boolean JsValue at address in `base_reg` with bool in `bool_reg` (w).
    /// Assumes the slot has already been zeroed.
    /// IMPORTANT: writes bool byte first, then tag, to avoid clobbering `bool_reg`
    /// when it happens to be the same register as the scratch (x2).
    fn emit_write_boolean(&mut self, base_reg: u8, bool_reg: u8) {
        // Write bool byte first (in case bool_reg == X2)
        self.emit_strb(bool_reg, base_reg, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_movz_w(reg::X2, layout::TAG_BOOLEAN as u16);
        self.emit_strb(reg::X2, base_reg, layout::JSVALUE_TAG_OFFSET as u32);
    }

    /// Decrement len by 1 and store back: x12 = x12 - 1, str x12, [x10].
    fn emit_dec_len(&mut self) {
        self.emit_sub_imm_x(reg::X12, reg::X12, 1);
        self.emit_str_x(reg::X12, reg::X10, 0);
    }

    /// Increment len by 1 and store back: x12 = x12 + 1, str x12, [x10].
    fn emit_inc_len(&mut self) {
        self.emit_add_imm_x(reg::X12, reg::X12, 1);
        self.emit_str_x(reg::X12, reg::X10, 0);
    }

    /// Compute new slot address for push: x0 = ptr + len * 32.
    /// Assumes x11=ptr, x12=len already loaded.
    fn emit_compute_push_addr(&mut self) {
        self.emit_lsl_imm_x(reg::X0, reg::X12, 5);
        self.emit_add_shifted_x(reg::X0, reg::X11, reg::X0, 0);
    }

    // =============================================================
    // CallFrame access helpers
    // =============================================================

    /// Load the address of the last CallFrame into `dst_reg`.
    /// call_stack.last() = cs_ptr + (cs_len - 1) * callframe_size
    /// Uses scratch: x3, x4, x5.
    fn emit_load_last_frame_addr(&mut self, dst_reg: u8) {
        let cs_ptr_off = layout::vm_cs_ptr_offset();
        let cs_len_off = layout::vm_cs_len_offset();
        let cf_size = layout::callframe_size();

        // x3 = call_stack.ptr
        self.emit_ldr_x(reg::X3, reg::X19, cs_ptr_off as u32);
        // x4 = call_stack.len
        self.emit_ldr_x(reg::X4, reg::X19, cs_len_off as u32);
        // x4 = len - 1
        self.emit_sub_imm_x(reg::X4, reg::X4, 1);
        // x5 = callframe_size
        self.emit_mov_imm64(reg::X5, cf_size as u64);
        // x4 = (len - 1) * callframe_size — use MADD: Xd = Xa * Xb + Xc
        // madd x4, x4, x5, xzr
        let inst: u32 = 0x9B050000
            | ((reg::X5 as u32 & 0x1F) << 16) // Rm = x5
            | (31 << 10) // Ra = xzr
            | ((reg::X4 as u32 & 0x1F) << 5) // Rn = x4
            | (reg::X4 as u32 & 0x1F); // Rd = x4
        self.emit_u32(inst);
        // dst = cs_ptr + offset
        self.emit_add_shifted_x(dst_reg, reg::X3, reg::X4, 0);
    }

    /// Emit inline LoadLocal(idx): push locals[idx] from the current CallFrame.
    fn emit_inline_load_local(&mut self, stub_addr: usize, idx: u16) {
        let cf_locals_ptr = layout::cf_locals_ptr_delta();
        let cf_locals_len = layout::cf_locals_len_delta();

        // Load value_stack state for push
        self.emit_load_vs_state();

        // Check vs cap
        self.emit_cmp_reg_x(reg::X12, reg::X13);
        let slow_cap = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_HS as u32);

        // Get last frame addr → x5
        self.emit_load_last_frame_addr(reg::X5);

        // x6 = locals.ptr (frame + cf_locals_ptr offset)
        self.emit_ldr_x(reg::X0, reg::X5, cf_locals_ptr as u32);
        // x3 = locals.len
        self.emit_ldr_x(reg::X3, reg::X5, cf_locals_len as u32);

        // Bounds check: idx < locals.len
        self.emit_cmp_imm_x(reg::X3, (idx as u32) + 1);
        let slow_bounds = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // x0 = &locals[idx] = locals_ptr + idx * 32
        if idx > 0 {
            self.emit_add_imm_x(reg::X0, reg::X0, (idx as u32) * 32);
        }

        // Compute push addr: x1 = vs_ptr + vs_len * 32
        self.emit_lsl_imm_x(reg::X1, reg::X12, 5);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Copy 32 bytes from locals[idx] to push slot
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 0);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 0);
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 16);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 16);

        // If heap type, Rc::clone
        self.emit_ldrb(reg::X2, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_clone = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32);

        // Symbol → stub (too complex for clone)
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        self.emit_rc_clone_for_jsvalue(reg::X1, reg::X2);

        let skip_clone_target = self.code.len();

        // len++
        self.emit_inc_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_1(stub_addr, idx as u32);

        let end = self.code.len();

        self.patch_local_branch(slow_cap, slow_path);
        self.patch_local_branch(slow_bounds, slow_path);
        self.patch_local_branch(slow_sym, slow_path);
        self.patch_local_branch(skip_clone, skip_clone_target);
        self.patch_local_branch(skip_slow, end);
    }

    /// Emit inline StoreLocal(idx): pop TOS and move into locals[idx].
    fn emit_inline_store_local(&mut self, stub_addr: usize, idx: u16) {
        let cf_locals_ptr = layout::cf_locals_ptr_delta();
        let cf_locals_len = layout::cf_locals_len_delta();

        self.emit_load_vs_state_no_cap();

        // Check vs_len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // Get last frame addr → x5
        self.emit_load_last_frame_addr(reg::X5);

        // x0 = locals.ptr
        self.emit_ldr_x(reg::X0, reg::X5, cf_locals_ptr as u32);
        // x3 = locals.len
        self.emit_ldr_x(reg::X3, reg::X5, cf_locals_len as u32);

        // Bounds check: idx < locals.len
        self.emit_cmp_imm_x(reg::X3, (idx as u32) + 1);
        let slow_bounds = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // x0 = &locals[idx]
        if idx > 0 {
            self.emit_add_imm_x(reg::X0, reg::X0, (idx as u32) * 32);
        }

        // Save &locals[idx] in x20 (callee-saved) early
        self.emit_mov_reg(reg::X20, reg::X0);

        // Old value tag at locals[idx] — need Rc::drop if heap type
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_old_drop = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32);

        // Symbol → stub
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_sym_old = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Rc::drop old value
        self.emit_rc_drop_for_jsvalue(reg::X0, reg::X2);

        let skip_old_drop_target = self.code.len();

        // Reload vs state (emit_rc_drop and emit_load_last_frame_addr clobber scratch regs)
        self.emit_load_vs_state_no_cap();
        // x0 = TOS addr
        self.emit_compute_tos_addr();

        // Copy 32 bytes from TOS (x0) to locals[idx] (x20) — ownership transfer, no clone
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 0);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X20, 0);
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 16);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X20, 16);

        // Decrement value_stack len (pop)
        self.emit_dec_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_1(stub_addr, idx as u32);

        let end = self.code.len();

        self.patch_local_branch(slow_len, slow_path);
        self.patch_local_branch(slow_bounds, slow_path);
        self.patch_local_branch(slow_sym_old, slow_path);
        self.patch_local_branch(skip_old_drop, skip_old_drop_target);
        self.patch_local_branch(skip_slow, end);
    }

    /// Emit inline This: push call_stack.last().this_value.
    fn emit_inline_this(&mut self, stub_addr: usize) {
        let cf_this = layout::cf_this_offset();

        self.emit_load_vs_state();

        // Check cap
        self.emit_cmp_reg_x(reg::X12, reg::X13);
        let slow_cap = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_HS as u32);

        // Get last frame → x5
        self.emit_load_last_frame_addr(reg::X5);

        // x0 = &frame.this_value
        self.emit_add_imm_x(reg::X0, reg::X5, cf_this as u32);

        // Compute push addr: x1 = vs_ptr + vs_len * 32
        self.emit_lsl_imm_x(reg::X1, reg::X12, 5);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Copy 32 bytes
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 0);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 0);
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 16);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 16);

        // If heap type, Rc::clone
        self.emit_ldrb(reg::X2, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_clone = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32);

        // Symbol → stub
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        self.emit_rc_clone_for_jsvalue(reg::X1, reg::X2);

        let skip_clone_target = self.code.len();

        // len++
        self.emit_inc_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_cap, slow_path);
        self.patch_local_branch(slow_sym, slow_path);
        self.patch_local_branch(skip_clone, skip_clone_target);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Rc refcount helpers for heap types (String, Object)
    // =============================================================

    /// Increment the strong count of an Rc whose RcBox pointer is in `rcbox_reg`.
    /// Uses scratch registers x3, x4.
    ///
    /// ```asm
    /// ldr x4, [rcbox_reg, #strong_offset]
    /// add x4, x4, #1
    /// str x4, [rcbox_reg, #strong_offset]
    /// ```
    fn emit_rc_inc(&mut self, rcbox_reg: u8) {
        let strong_off = layout::rc_strong_offset() as u32;
        self.emit_ldr_x(reg::X4, rcbox_reg, strong_off);
        self.emit_add_imm_x(reg::X4, reg::X4, 1);
        self.emit_str_x(reg::X4, rcbox_reg, strong_off);
    }

    /// Decrement the strong count of an Rc whose RcBox pointer is in `rcbox_reg`.
    /// Does NOT free on zero — GC handles deallocation.
    /// Uses scratch registers x4.
    ///
    /// ```asm
    /// ldr x4, [rcbox_reg, #strong_offset]
    /// sub x4, x4, #1
    /// str x4, [rcbox_reg, #strong_offset]
    /// ```
    fn emit_rc_dec(&mut self, rcbox_reg: u8) {
        let strong_off = layout::rc_strong_offset() as u32;
        self.emit_ldr_x(reg::X4, rcbox_reg, strong_off);
        self.emit_sub_imm_x(reg::X4, reg::X4, 1);
        self.emit_str_x(reg::X4, rcbox_reg, strong_off);
    }

    /// Load the RcBox pointer for a JsValue::String at address `jsval_reg`.
    /// Rc<str> is a fat pointer: (data_ptr, len) at offsets 8 and 16.
    /// RcBox ptr = data_ptr - str_data_back_offset.
    /// Result in `dst_reg`.
    fn emit_load_string_rcbox(&mut self, dst_reg: u8, jsval_reg: u8) {
        let back = layout::rc_str_data_back_offset() as u32;
        // x3 = data_ptr (at JsValue offset 8)
        self.emit_ldr_x(dst_reg, jsval_reg, 8);
        // dst = data_ptr - back_offset
        self.emit_sub_imm_x(dst_reg, dst_reg, back);
    }

    /// Load the RcBox pointer for a JsValue::Object at address `jsval_reg`.
    /// GcPtr<JsObject> holds Rc<RefCell<JsObject>> — a thin pointer at offset 8.
    /// That thin pointer IS the RcBox pointer.
    /// Result in `dst_reg`.
    fn emit_load_object_rcbox(&mut self, dst_reg: u8, jsval_reg: u8) {
        // x3 = Rc ptr (at JsValue offset 8) = RcBox ptr
        self.emit_ldr_x(dst_reg, jsval_reg, 8);
    }

    /// Emit Rc::clone for the JsValue at `jsval_reg`, dispatching by tag in `tag_reg`.
    /// Handles String (tag=4) and Object (tag=6). Symbol (tag=5) is NOT handled here.
    /// Caller must ensure tag > 3 and tag != 5 before calling, or provide a skip branch.
    ///
    /// Uses scratch: x3, x4.
    fn emit_rc_clone_for_jsvalue(&mut self, jsval_reg: u8, tag_reg: u8) {
        // if tag == TAG_STRING (4)
        self.emit_cmp_imm(tag_reg, layout::TAG_STRING as u32);
        let string_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → string

        // if tag == TAG_OBJECT (6)
        self.emit_cmp_imm(tag_reg, layout::TAG_OBJECT as u32);
        let object_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → object

        // else (Symbol) → skip (should not reach here if caller filters)
        let skip_branch = self.code.len();
        self.emit_u32(0x14000000); // B → done

        // --- String path ---
        let string_path = self.code.len();
        self.emit_load_string_rcbox(reg::X3, jsval_reg);
        self.emit_rc_inc(reg::X3);
        let string_done = self.code.len();
        self.emit_u32(0x14000000); // B → done

        // --- Object path ---
        let object_path = self.code.len();
        self.emit_load_object_rcbox(reg::X3, jsval_reg);
        self.emit_rc_inc(reg::X3);
        // fall through to done

        let done = self.code.len();

        self.patch_local_branch(string_branch, string_path);
        self.patch_local_branch(object_branch, object_path);
        self.patch_local_branch(skip_branch, done);
        self.patch_local_branch(string_done, done);
    }

    /// Emit Rc::drop (decrement) for the JsValue at `jsval_reg`, dispatching by tag in `tag_reg`.
    /// Same dispatch as emit_rc_clone_for_jsvalue but decrements.
    fn emit_rc_drop_for_jsvalue(&mut self, jsval_reg: u8, tag_reg: u8) {
        // if tag == TAG_STRING (4)
        self.emit_cmp_imm(tag_reg, layout::TAG_STRING as u32);
        let string_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // if tag == TAG_OBJECT (6)
        self.emit_cmp_imm(tag_reg, layout::TAG_OBJECT as u32);
        let object_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // else (Symbol) → skip
        let skip_branch = self.code.len();
        self.emit_u32(0x14000000);

        // --- String path ---
        let string_path = self.code.len();
        self.emit_load_string_rcbox(reg::X3, jsval_reg);
        self.emit_rc_dec(reg::X3);
        let string_done = self.code.len();
        self.emit_u32(0x14000000);

        // --- Object path ---
        let object_path = self.code.len();
        self.emit_load_object_rcbox(reg::X3, jsval_reg);
        self.emit_rc_dec(reg::X3);
        // fall through

        let done = self.code.len();

        self.patch_local_branch(string_branch, string_path);
        self.patch_local_branch(object_branch, object_path);
        self.patch_local_branch(skip_branch, done);
        self.patch_local_branch(string_done, done);
    }

    // =============================================================
    // Inline push literals (Undefined, Null, True, False) — fully inline
    // =============================================================

    /// Emit fully inlined push literal.
    /// Pattern: check cap, compute slot, zero 32 bytes, write tag (+ bool), len++.
    fn emit_inline_push_literal(&mut self, stub_addr: usize, tag: u8, bool_val: Option<u8>) {
        self.emit_load_vs_state();

        // Check len < cap
        self.emit_cmp_imm_x(reg::X12, 0); // dummy — we use cmp_reg
                                          // Actually: cmp x12, x13
        self.emit_cmp_reg_x(reg::X12, reg::X13);
        let slow_branch = self.code.len();
        // b.hs stub (len >= cap → stub)
        self.emit_u32(0x54000000 | Self::COND_HS as u32);

        // x0 = ptr + len * 32 (new slot)
        self.emit_compute_push_addr();

        // Zero 32 bytes
        self.emit_zero_jsvalue(reg::X0);

        // Write tag
        if tag != 0 {
            // tag 0 = Undefined, already zero from stp
            self.emit_movz_w(reg::X2, tag as u16);
            self.emit_strb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        }

        // Write bool if Boolean
        if let Some(bv) = bool_val {
            self.emit_movz_w(reg::X3, bv as u16);
            self.emit_strb(reg::X3, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        }

        // len++
        self.emit_inc_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // Slow path
        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Pop — primitive fast path
    // =============================================================

    /// Pop: if TOS tag <= 3 (primitive), just len--.
    /// If tag == 4 (String) or 6 (Object), Rc::drop then len--.
    /// If tag == 5 (Symbol), fall to stub (complex drop).
    fn emit_inline_pop(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // Load TOS tag
        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // tag <= 3 → primitive fast path (just len--)
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let prim_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS → prim_path

        // tag == 5 (Symbol) → stub (has Option<Rc<str>> inside, complex)
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_branch_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → stub

        // tag == 4 (String) or 6 (Object) → Rc drop + len--
        self.emit_rc_drop_for_jsvalue(reg::X0, reg::X2);
        // fall through to prim_path (len--)

        // --- Primitive / post-Rc-drop path: just decrement len ---
        let prim_path = self.code.len();
        self.emit_dec_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(prim_branch, prim_path);
        self.patch_local_branch(slow_branch_sym, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Dup — primitive fast path
    // =============================================================

    /// Dup: copy TOS 32 bytes to new slot, len++.
    /// If heap type (String/Object), also Rc::clone. Symbol → stub.
    fn emit_inline_dup(&mut self, stub_addr: usize) {
        self.emit_load_vs_state();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        // Check len < cap
        self.emit_cmp_reg_x(reg::X12, reg::X13);
        let slow_branch_cap = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_HS as u32);

        // Load TOS tag
        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // tag == 5 (Symbol) → stub
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_branch_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Copy 32 bytes: ldp + stp × 2
        // x1 = new slot = ptr + len * 32
        self.emit_lsl_imm_x(reg::X1, reg::X12, 5);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // ldp x2, x3, [x0, #0]; stp x2, x3, [x1, #0]
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 0);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 0);
        // ldp x2, x3, [x0, #16]; stp x2, x3, [x1, #16]
        self.emit_ldp_offset(reg::X2, reg::X3, reg::X0, 16);
        self.emit_stp_offset(reg::X2, reg::X3, reg::X1, 16);

        // If heap type (tag > 3), Rc::clone on the new slot
        // Reload tag from x1 (new slot) since x2 was clobbered by ldp
        self.emit_ldrb(reg::X2, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_clone = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS → skip (primitive)

        // Rc::clone for new slot
        self.emit_rc_clone_for_jsvalue(reg::X1, reg::X2);

        let skip_clone_target = self.code.len();

        // len++
        self.emit_inc_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_cap, slow_path);
        self.patch_local_branch(slow_branch_sym, slow_path);
        self.patch_local_branch(skip_clone, skip_clone_target);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Void — primitive fast path
    // =============================================================

    /// Void: Rc::drop if heap type, then overwrite TOS with Undefined (zero 32 bytes).
    /// Symbol → stub.
    fn emit_inline_void(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // tag == 5 (Symbol) → stub
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_branch_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // tag > 3 → Rc::drop before zeroing
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_drop = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS → skip_drop (primitive)

        self.emit_rc_drop_for_jsvalue(reg::X0, reg::X2);

        let skip_drop_target = self.code.len();

        // Zero the 32 bytes → becomes Undefined (tag=0)
        self.emit_zero_jsvalue(reg::X0);

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_sym, slow_path);
        self.patch_local_branch(skip_drop, skip_drop_target);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Delete — primitive fast path
    // =============================================================

    /// Delete: Rc::drop if heap type, then overwrite TOS with Boolean(true).
    /// Symbol → stub.
    fn emit_inline_delete(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // tag == 5 (Symbol) → stub
        self.emit_cmp_imm(reg::X2, layout::TAG_SYMBOL as u32);
        let slow_branch_sym = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // tag > 3 → Rc::drop before overwriting
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let skip_drop = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS → skip_drop

        self.emit_rc_drop_for_jsvalue(reg::X0, reg::X2);

        let skip_drop_target = self.code.len();

        // Zero then write Boolean(true)
        self.emit_zero_jsvalue(reg::X0);
        self.emit_movz_w(reg::X2, 1); // true
        self.emit_write_boolean(reg::X0, reg::X2);

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_sym, slow_path);
        self.patch_local_branch(skip_drop, skip_drop_target);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Not — all primitives
    // =============================================================

    /// Not: Boolean→flip, Number(0/NaN)→true else→false, Undefined/Null→true, else→stub.
    fn emit_inline_not(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // Dispatch by tag
        // Boolean (2)
        self.emit_cmp_imm(reg::X2, layout::TAG_BOOLEAN as u32);
        let bool_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Number (3)
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let number_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Undefined(0) or Null(1): tag <= 1 → true
        self.emit_cmp_imm(reg::X2, 1);
        let undef_null_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LS as u32); // B.LS

        // else → stub (heap type)
        let to_stub = self.code.len();
        self.emit_u32(0x14000000);

        // --- Boolean path: flip ---
        let bool_path = self.code.len();
        self.emit_ldrb(reg::X3, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_eor_imm_w(reg::X3, reg::X3, 1);
        self.emit_strb(reg::X3, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        let bool_done = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // --- Number path: !0 → true, !NaN → true, else → false ---
        let number_path = self.code.len();
        self.emit_ldr_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        // fcmp d0, #0.0 sets: Z if zero, VS if NaN (unordered)
        self.emit_fcmp_d_zero(reg::D0);
        // If EQ (zero) or VS (NaN) → true, else → false
        // CSET w3, EQ (zero)
        self.emit_cset_w(reg::X3, Self::COND_EQ);
        // CSET w4, VS (NaN)
        self.emit_cset_w(reg::X4, Self::COND_VS);
        // w3 = w3 | w4 (true if zero or NaN)
        self.emit_orr_reg_w(reg::X3, reg::X3, reg::X4);
        // Now overwrite TOS with Boolean(w3)
        self.emit_zero_jsvalue(reg::X0);
        self.emit_write_boolean(reg::X0, reg::X3);
        let number_done = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // --- Undefined/Null path: always true ---
        let undef_null_path = self.code.len();
        self.emit_zero_jsvalue(reg::X0);
        self.emit_movz_w(reg::X3, 1);
        self.emit_write_boolean(reg::X0, reg::X3);
        let undef_null_done = self.code.len();
        self.emit_u32(0x14000000); // B → end

        // --- Stub path ---
        let stub_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, stub_path);
        self.patch_local_branch(bool_branch, bool_path);
        self.patch_local_branch(number_branch, number_path);
        self.patch_local_branch(undef_null_branch, undef_null_path);
        self.patch_local_branch(to_stub, stub_path);
        self.patch_local_branch(bool_done, end);
        self.patch_local_branch(number_done, end);
        self.patch_local_branch(undef_null_done, end);
    }

    // =============================================================
    // Inline Pos — all primitives
    // =============================================================

    /// Pos: Number→noop, Boolean→Number(0/1), Null→Number(0), Undefined→Number(NaN), else→stub.
    fn emit_inline_pos(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        // Check len >= 1
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        // Number (3): noop
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let number_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32); // B.EQ → end (noop)

        // Boolean (2): convert to Number(0.0 or 1.0)
        self.emit_cmp_imm(reg::X2, layout::TAG_BOOLEAN as u32);
        let bool_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Null (1): Number(0.0)
        self.emit_cmp_imm(reg::X2, layout::TAG_NULL as u32);
        let null_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // Undefined (0): Number(NaN)
        self.emit_cmp_imm(reg::X2, layout::TAG_UNDEFINED as u32);
        let undef_branch = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_EQ as u32);

        // else → stub
        let to_stub = self.code.len();
        self.emit_u32(0x14000000);

        // --- Boolean path: bool → Number ---
        let bool_path = self.code.len();
        self.emit_ldrb(reg::X3, reg::X0, layout::JSVALUE_BOOL_OFFSET as u32);
        self.emit_ucvtf_d_w(reg::D0, reg::X3); // 0 or 1 → 0.0 or 1.0
        self.emit_zero_jsvalue(reg::X0);
        self.emit_write_number(reg::X0, reg::D0);
        let bool_done = self.code.len();
        self.emit_u32(0x14000000);

        // --- Null path: Number(0.0) ---
        let null_path = self.code.len();
        self.emit_zero_jsvalue(reg::X0);
        // tag=3, f64=0.0 (all zeros at f64 offset is 0.0)
        self.emit_movz_w(reg::X2, layout::TAG_NUMBER as u16);
        self.emit_strb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        let null_done = self.code.len();
        self.emit_u32(0x14000000);

        // --- Undefined path: Number(NaN) ---
        let undef_path = self.code.len();
        // Load NaN into d0: canonical NaN = 0x7FF8000000000000
        self.emit_mov_imm64(reg::X3, 0x7FF8_0000_0000_0000u64);
        self.emit_zero_jsvalue(reg::X0);
        self.emit_movz_w(reg::X2, layout::TAG_NUMBER as u16);
        self.emit_strb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        // Store NaN as raw bits at f64 offset
        self.emit_str_x(reg::X3, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        let undef_done = self.code.len();
        self.emit_u32(0x14000000);

        // --- Stub path ---
        let stub_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, stub_path);
        self.patch_local_branch(number_branch, end); // noop
        self.patch_local_branch(bool_branch, bool_path);
        self.patch_local_branch(null_branch, null_path);
        self.patch_local_branch(undef_branch, undef_path);
        self.patch_local_branch(to_stub, stub_path);
        self.patch_local_branch(bool_done, end);
        self.patch_local_branch(null_done, end);
        self.patch_local_branch(undef_done, end);
    }

    // =============================================================
    // Inline BitNot — Number fast path
    // =============================================================

    /// BitNot: Number → fcvtzs→mvn→scvtf. Else stub.
    fn emit_inline_bitnot(&mut self, stub_addr: usize) {
        self.emit_load_vs_state_no_cap();

        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch_tag = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // Fast: ldr d0 → fcvtzs w2, d0 → mvn w2, w2 → scvtf d0, w2 → str d0
        self.emit_ldr_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_fcvtzs_d_w(reg::X2, reg::D0);
        self.emit_mvn_w(reg::X2, reg::X2);
        self.emit_scvtf_d_w(reg::D0, reg::X2);
        self.emit_str_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_tag, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline binary bitwise ops (BitAnd, BitOr, BitXor)
    // =============================================================

    /// Binary bitwise: both Number → fcvtzs→op→scvtf. Else stub.
    fn emit_inline_bitwise_binop(&mut self, stub_addr: usize, op: fn(&mut Self, u8, u8, u8)) {
        self.emit_load_vs_state_no_cap();

        self.emit_cmp_imm_x(reg::X12, 2);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos2_addrs();

        // Load tags
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        // Both Number?
        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch_rhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        self.emit_cmp_imm(reg::X3, layout::TAG_NUMBER as u32);
        let slow_branch_lhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // Fast path
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_fcvtzs_d_w(reg::X2, reg::D0); // lhs i32
        self.emit_fcvtzs_d_w(reg::X3, reg::D1); // rhs i32
        op(self, reg::X2, reg::X2, reg::X3);
        self.emit_scvtf_d_w(reg::D0, reg::X2);
        self.emit_str_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);

        // len--
        self.emit_dec_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_rhs, slow_path);
        self.patch_local_branch(slow_branch_lhs, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline shift ops (Shl, Shr, UShr)
    // =============================================================

    /// Shift: both Number → fcvtzs, mask shift amount to 5 bits, shift, scvtf/ucvtf.
    fn emit_inline_shift(
        &mut self,
        stub_addr: usize,
        shift_op: fn(&mut Self, u8, u8, u8),
        unsigned_result: bool,
    ) {
        self.emit_load_vs_state_no_cap();

        self.emit_cmp_imm_x(reg::X12, 2);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos2_addrs();

        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch_rhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        self.emit_cmp_imm(reg::X3, layout::TAG_NUMBER as u32);
        let slow_branch_lhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // Fast path
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32); // rhs
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32); // lhs
        self.emit_fcvtzs_d_w(reg::X2, reg::D0); // lhs i32
        self.emit_fcvtzs_d_w(reg::X3, reg::D1); // rhs i32
                                                // Mask shift amount to 0x1F
        self.emit_and_imm_w(reg::X3, reg::X3, 0x1F);
        // Perform shift
        shift_op(self, reg::X2, reg::X2, reg::X3);
        // Convert back to f64
        if unsigned_result {
            self.emit_ucvtf_d_w(reg::D0, reg::X2);
        } else {
            self.emit_scvtf_d_w(reg::D0, reg::X2);
        }
        self.emit_str_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);

        self.emit_dec_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_rhs, slow_path);
        self.patch_local_branch(slow_branch_lhs, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline PostfixInc/Dec — Number fast path
    // =============================================================

    /// PostfixInc/Dec: pop 1, push 2 (old + new). Number only.
    fn emit_inline_postfix(&mut self, stub_addr: usize, is_inc: bool) {
        self.emit_load_vs_state();

        // Check len >= 1 AND len < cap (net push = +1)
        self.emit_cmp_imm_x(reg::X12, 1);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_cmp_reg_x(reg::X12, reg::X13);
        let slow_branch_cap = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_HS as u32);

        self.emit_compute_tos_addr();
        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);

        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch_tag = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // d0 = old value (stays at TOS)
        self.emit_ldr_d(reg::D0, reg::X0, layout::JSVALUE_F64_OFFSET as u32);

        // Load 1.0 into d1
        // 1.0 = 0x3FF0000000000000
        self.emit_mov_imm64(reg::X3, 0x3FF0_0000_0000_0000u64);
        // fmov d1, x3
        self.emit_fmov_d_x(reg::D1, reg::X3);

        // d2 = old +/- 1.0
        if is_inc {
            self.emit_fadd_d(reg::D2, reg::D0, reg::D1);
        } else {
            self.emit_fsub_d(reg::D2, reg::D0, reg::D1);
        }

        // New slot at ptr + len * 32
        self.emit_lsl_imm_x(reg::X1, reg::X12, 5);
        self.emit_add_shifted_x(reg::X1, reg::X11, reg::X1, 0);

        // Write new value to new slot
        self.emit_zero_jsvalue(reg::X1);
        self.emit_write_number(reg::X1, reg::D2);

        // len++
        self.emit_inc_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_cap, slow_path);
        self.patch_local_branch(slow_branch_tag, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Mod/Exp — Number + libc direct call
    // =============================================================

    /// Mod/Exp: both Number → call libc fmod/pow directly (not through Rust stub).
    fn emit_inline_libc_binop(&mut self, stub_addr: usize, libc_fn_addr: usize) {
        self.emit_load_vs_state_no_cap();

        self.emit_cmp_imm_x(reg::X12, 2);
        let slow_branch_len = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_LO as u32);

        self.emit_compute_tos2_addrs();

        self.emit_ldrb(reg::X2, reg::X0, layout::JSVALUE_TAG_OFFSET as u32);
        self.emit_ldrb(reg::X3, reg::X1, layout::JSVALUE_TAG_OFFSET as u32);

        self.emit_cmp_imm(reg::X2, layout::TAG_NUMBER as u32);
        let slow_branch_rhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        self.emit_cmp_imm(reg::X3, layout::TAG_NUMBER as u32);
        let slow_branch_lhs = self.code.len();
        self.emit_u32(0x54000000 | Self::COND_NE as u32);

        // Save x0, x1 (TOS addrs) — we'll need x1 after the call
        // Use x20 (callee-saved, already saved in prologue) for lhs addr
        self.emit_mov_reg(reg::X20, reg::X1);

        // d0 = lhs.f64, d1 = rhs.f64 (aarch64 calling convention)
        self.emit_ldr_d(reg::D0, reg::X1, layout::JSVALUE_F64_OFFSET as u32);
        self.emit_ldr_d(reg::D1, reg::X0, layout::JSVALUE_F64_OFFSET as u32);

        // blr to libc function
        self.emit_mov_imm64(reg::X9, libc_fn_addr as u64);
        let blr_inst: u32 = 0xD63F0000 | ((reg::X9 as u32 & 0x1F) << 5);
        self.emit_u32(blr_inst);

        // Result in d0; write back to lhs slot
        self.emit_str_d(reg::D0, reg::X20, layout::JSVALUE_F64_OFFSET as u32);

        // Re-load vs state for len-- (blr may have clobbered x10, x12)
        let vs_len_off = layout::vm_vs_len_offset();
        self.emit_ldr_x(reg::X12, reg::X19, vs_len_off as u32);
        self.emit_add_imm_x(reg::X10, reg::X19, vs_len_off as u32);
        self.emit_dec_len();

        let skip_slow = self.code.len();
        self.emit_u32(0x14000000);

        let slow_path = self.code.len();
        self.emit_stub_call_0(stub_addr);

        let end = self.code.len();

        self.patch_local_branch(slow_branch_len, slow_path);
        self.patch_local_branch(slow_branch_rhs, slow_path);
        self.patch_local_branch(slow_branch_lhs, slow_path);
        self.patch_local_branch(skip_slow, end);
    }

    // =============================================================
    // Inline Add — Number fast path (same as Sub but fadd)
    // =============================================================

    /// `fmov Dd, Xn` -- move GPR to FPR (64-bit).
    fn emit_fmov_d_x(&mut self, dd: u8, xn: u8) {
        // FMOV Dd, Xn: 0x9E670000
        let inst: u32 = 0x9E670000 | ((xn as u32 & 0x1F) << 5) | (dd as u32 & 0x1F);
        self.emit_u32(inst);
    }

    /// `cmp Xn, Xm` -- 64-bit register compare.
    fn emit_cmp_reg_x(&mut self, rn: u8, rm: u8) {
        // SUBS XZR, Xn, Xm
        let inst: u32 = 0xEB00001F | ((rm as u32 & 0x1F) << 16) | ((rn as u32 & 0x1F) << 5);
        self.emit_u32(inst);
    }

    // Condition code: HS = unsigned higher or same (carry set) = 0x2
    const COND_HS: u8 = 0x2;
    #[allow(dead_code)]
    const COND_HI: u8 = 0x8; // unsigned higher
    const COND_VS: u8 = 0x6; // overflow (used for NaN in fcmp)

    /// Patch a local branch instruction at `branch_offset` to jump to `target_offset`.
    fn patch_local_branch(&mut self, branch_offset: usize, target_offset: usize) {
        let delta = ((target_offset as i64) - (branch_offset as i64)) / 4;
        let existing = u32::from_le_bytes([
            self.code[branch_offset],
            self.code[branch_offset + 1],
            self.code[branch_offset + 2],
            self.code[branch_offset + 3],
        ]);

        let patched = if existing & 0xFC000000 == 0x14000000 {
            // Unconditional branch B
            let imm26 = (delta as u32) & 0x03FFFFFF;
            0x14000000 | imm26
        } else {
            // Conditional branch B.cond
            let cond = existing & 0xF;
            let imm19 = ((delta as u32) & 0x7FFFF) << 5;
            0x54000000 | imm19 | cond
        };

        let bytes = patched.to_le_bytes();
        self.code[branch_offset] = bytes[0];
        self.code[branch_offset + 1] = bytes[1];
        self.code[branch_offset + 2] = bytes[2];
        self.code[branch_offset + 3] = bytes[3];
    }
}

// -----------------------------------------------------------------
// JitFunction
// -----------------------------------------------------------------

/// A compiled native function backed by mmap'd executable memory.
pub struct JitFunction {
    pub(crate) code: *const u8,
    size: usize,
}

unsafe impl Send for JitFunction {}
unsafe impl Sync for JitFunction {}

impl JitFunction {
    /// Call the JIT-compiled function with a VM pointer.
    ///
    /// Returns: 0 = success, 1 = error
    ///
    /// # Safety
    /// The caller must ensure `vm` is a valid, non-null pointer to a `Vm`
    /// that remains valid for the duration of the call.
    pub unsafe fn call_vm(&self, vm: *mut Vm) -> u32 {
        let func: extern "C" fn(*mut Vm) -> u32 = std::mem::transmute(self.code);
        func(vm)
    }
}

impl Drop for JitFunction {
    fn drop(&mut self) {
        if !self.code.is_null() && self.size > 0 {
            unsafe {
                munmap(self.code as *mut u8, self.size);
            }
        }
    }
}
