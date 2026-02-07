// === Generator examples ===

// --- 1. Basic generator ---
function* count() {
  yield 1;
  yield 2;
  yield 3;
}

let gen = count();
console.log("1. Basic generator:");
let r1 = gen.next();
console.log("  value:", r1.value, "done:", r1.done);
let r2 = gen.next();
console.log("  value:", r2.value, "done:", r2.done);
let r3 = gen.next();
console.log("  value:", r3.value, "done:", r3.done);
let r4 = gen.next();
console.log("  value:", r4.value, "done:", r4.done);

// --- 2. Generator with return value ---
function* withReturn() {
  yield "a";
  return "done!";
}

console.log("\n2. Generator with return:");
let gen2 = withReturn();
let s1 = gen2.next();
console.log("  value:", s1.value, "done:", s1.done);
let s2 = gen2.next();
console.log("  value:", s2.value, "done:", s2.done);
let s3 = gen2.next();
console.log("  value:", s3.value, "done:", s3.done);

// --- 3. Generator .return() ---
function* earlyReturn() {
  yield 1;
  yield 2;
  yield 3;
}

console.log("\n3. Generator .return():");
let gen3 = earlyReturn();
let t1 = gen3.next();
console.log("  next: value:", t1.value, "done:", t1.done);
let t2 = gen3.return(42);
console.log("  return(42): value:", t2.value, "done:", t2.done);
let t3 = gen3.next();
console.log("  next: value:", t3.value, "done:", t3.done);

// --- 4. Generator for-of ---
function* range(start, end) {
  for (let i = start; i < end; i++) {
    yield i;
  }
}

console.log("\n4. Generator for-of:");
for (let val of range(1, 5)) {
  console.log("  value:", val);
}

// --- 5. Fibonacci generator ---
function* fibonacci() {
  let a = 0;
  let b = 1;
  while (true) {
    yield a;
    let temp = a;
    a = b;
    b = temp + b;
  }
}

console.log("\n5. Fibonacci (first 8):");
let fib = fibonacci();
for (let i = 0; i < 8; i++) {
  let r = fib.next();
  console.log("  fib:", r.value);
}

// --- 6. Passing values to next() ---
function* accumulator() {
  let sum = 0;
  while (true) {
    let value = yield sum;
    sum = sum + value;
  }
}

console.log("\n6. Passing values to next():");
let acc = accumulator();
let a1 = acc.next();
console.log("  next(): value:", a1.value);
let a2 = acc.next(10);
console.log("  next(10): value:", a2.value);
let a3 = acc.next(20);
console.log("  next(20): value:", a3.value);
let a4 = acc.next(5);
console.log("  next(5): value:", a4.value);
