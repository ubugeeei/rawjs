// ============================================================
// rawjs - Promise Examples
// ============================================================

// --------------------------------------------------
// 1. Promise.resolve + then
// --------------------------------------------------
console.log("=== 1. Promise.resolve ===");

let p1 = Promise.resolve(42);
p1.then(function (value) {
  console.log("Resolved:", value);
});

// --------------------------------------------------
// 2. Promise.reject + catch
// --------------------------------------------------
console.log("=== 2. Promise.reject ===");

let p2 = Promise.reject("something went wrong");
p2.catch(function (reason) {
  console.log("Rejected:", reason);
});

// --------------------------------------------------
// 3. Promise Constructor (resolve)
// --------------------------------------------------
console.log("=== 3. Promise Constructor ===");

let p3 = new Promise(function (resolve, reject) {
  console.log("Executor runs synchronously");
  resolve("hello from constructor");
});

p3.then(function (value) {
  console.log("Then:", value);
});

// --------------------------------------------------
// 4. Promise Constructor (reject)
// --------------------------------------------------
console.log("=== 4. Reject in Constructor ===");

let p4 = new Promise(function (resolve, reject) {
  reject("constructor rejection");
});

p4.catch(function (reason) {
  console.log("Caught rejection:", reason);
});

// --------------------------------------------------
// 5. Executor throw becomes rejection
// --------------------------------------------------
console.log("=== 5. Executor Throw ===");

let p5 = new Promise(function (resolve, reject) {
  throw new Error("executor error");
});

p5.catch(function (err) {
  console.log("Executor throw caught:", err.message);
});

// --------------------------------------------------
// 6. Multiple then handlers
// --------------------------------------------------
console.log("=== 6. Multiple Handlers ===");

let shared = Promise.resolve("shared value");
shared.then(function (v) {
  console.log("Handler A:", v);
});
shared.then(function (v) {
  console.log("Handler B:", v);
});

// --------------------------------------------------
// 7. Already resolved/rejected: then runs asynchronously
// --------------------------------------------------
console.log("=== 7. Async Execution Order ===");
console.log("Before then");
Promise.resolve("async").then(function (v) {
  console.log("In then:", v);
});
console.log("After then (should print before 'In then')");

console.log("--- Promise examples done ---");
