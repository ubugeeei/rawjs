// ============================================================
// rawjs - ESM Module: import demo
// ============================================================

// Default import + named imports from one module
import subtract, { add, multiply, square, PI, E } from "./math.js";

// Named imports from another module
import { capitalize, repeat, range } from "./utils.js";

// --------------------------------------------------
// 1. Math module
// --------------------------------------------------
console.log("=== Math Module ===");
console.log("add(2, 3) =", add(2, 3));
console.log("subtract(10, 3) =", subtract(10, 3));
console.log("multiply(4, 5) =", multiply(4, 5));
console.log("square(7) =", square(7));
console.log("PI =", PI);
console.log("E =", E);

// Compose functions
console.log("add(multiply(2,3), square(4)) =", add(multiply(2, 3), square(4)));

// --------------------------------------------------
// 2. Utils module
// --------------------------------------------------
console.log("=== Utils Module ===");
console.log("capitalize('hello') =", capitalize("hello"));
console.log("repeat('abc', 3) =", repeat("abc", 3));
console.log("range(1, 6) =", range(1, 6));

// Use range with for-of
let nums = range(1, 5);
let total = 0;
for (let n of nums) {
  total = total + n;
}
console.log("sum of range(1,5) =", total);

console.log("--- Module examples done ---");
