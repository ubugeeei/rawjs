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
    // Scratch registers
    pub const X9: u8 = 9;
    pub const X10: u8 = 10;
    // Callee-saved
    pub const X19: u8 = 19;
    pub const X20: u8 = 20;
    // Frame pointer & link register
    pub const X29: u8 = 29; // FP
    pub const X30: u8 = 30; // LR
    pub const SP: u8 = 31; // stack pointer (encoded as 31 in some insns)
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
    fn is_jittable(_chunk: &Chunk) -> bool {
        // All instructions are now JIT-compilable via stubs.
        true
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
                self.emit_stub_call_1(stubs::stub_load_local as *const () as usize, idx as u32);
            }
            Instruction::StoreLocal(idx) => {
                self.emit_stub_call_1(stubs::stub_store_local as *const () as usize, idx as u32);
            }
            Instruction::LoadGlobal(idx) => {
                self.emit_stub_call_1(stubs::stub_load_global as *const () as usize, idx as u32);
            }
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
                self.emit_stub_call_0(stubs::stub_pop as *const () as usize);
            }
            Instruction::Dup => {
                self.emit_stub_call_0(stubs::stub_dup as *const () as usize);
            }
            Instruction::Undefined => {
                self.emit_stub_call_0(stubs::stub_push_undefined as *const () as usize);
            }
            Instruction::Null => {
                self.emit_stub_call_0(stubs::stub_push_null as *const () as usize);
            }
            Instruction::True => {
                self.emit_stub_call_0(stubs::stub_push_true as *const () as usize);
            }
            Instruction::False => {
                self.emit_stub_call_0(stubs::stub_push_false as *const () as usize);
            }
            Instruction::This => {
                self.emit_stub_call_0(stubs::stub_push_this as *const () as usize);
            }

            // --- Arithmetic ---
            Instruction::Add => self.emit_stub_call_0(stubs::stub_add as *const () as usize),
            Instruction::Sub => self.emit_stub_call_0(stubs::stub_sub as *const () as usize),
            Instruction::Mul => self.emit_stub_call_0(stubs::stub_mul as *const () as usize),
            Instruction::Div => self.emit_stub_call_0(stubs::stub_div as *const () as usize),
            Instruction::Mod => self.emit_stub_call_0(stubs::stub_modulo as *const () as usize),
            Instruction::Exp => self.emit_stub_call_0(stubs::stub_exp as *const () as usize),
            Instruction::Neg => self.emit_stub_call_0(stubs::stub_neg as *const () as usize),
            Instruction::Pos => self.emit_stub_call_0(stubs::stub_pos as *const () as usize),
            Instruction::Not => self.emit_stub_call_0(stubs::stub_not as *const () as usize),
            Instruction::BitNot => self.emit_stub_call_0(stubs::stub_bit_not as *const () as usize),
            Instruction::TypeOf => self.emit_stub_call_0(stubs::stub_typeof as *const () as usize),
            Instruction::Void => self.emit_stub_call_0(stubs::stub_void as *const () as usize),
            Instruction::Delete => self.emit_stub_call_0(stubs::stub_delete as *const () as usize),

            // --- Comparison ---
            Instruction::Eq => self.emit_stub_call_0(stubs::stub_eq as *const () as usize),
            Instruction::StrictEq => {
                self.emit_stub_call_0(stubs::stub_strict_eq as *const () as usize)
            }
            Instruction::Ne => self.emit_stub_call_0(stubs::stub_ne as *const () as usize),
            Instruction::StrictNe => {
                self.emit_stub_call_0(stubs::stub_strict_ne as *const () as usize)
            }
            Instruction::Lt => self.emit_stub_call_0(stubs::stub_lt as *const () as usize),
            Instruction::Le => self.emit_stub_call_0(stubs::stub_le as *const () as usize),
            Instruction::Gt => self.emit_stub_call_0(stubs::stub_gt as *const () as usize),
            Instruction::Ge => self.emit_stub_call_0(stubs::stub_ge as *const () as usize),

            // --- Bitwise ---
            Instruction::BitAnd => self.emit_stub_call_0(stubs::stub_bit_and as *const () as usize),
            Instruction::BitOr => self.emit_stub_call_0(stubs::stub_bit_or as *const () as usize),
            Instruction::BitXor => self.emit_stub_call_0(stubs::stub_bit_xor as *const () as usize),
            Instruction::Shl => self.emit_stub_call_0(stubs::stub_shl as *const () as usize),
            Instruction::Shr => self.emit_stub_call_0(stubs::stub_shr as *const () as usize),
            Instruction::UShr => self.emit_stub_call_0(stubs::stub_ushr as *const () as usize),

            // --- Relational ---
            Instruction::In => self.emit_stub_call_0(stubs::stub_in as *const () as usize),
            Instruction::Instanceof => {
                self.emit_stub_call_0(stubs::stub_instanceof as *const () as usize)
            }

            // --- Postfix ---
            Instruction::PostfixIncrement => {
                self.emit_stub_call_0(stubs::stub_postfix_inc as *const () as usize)
            }
            Instruction::PostfixDecrement => {
                self.emit_stub_call_0(stubs::stub_postfix_dec as *const () as usize)
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
