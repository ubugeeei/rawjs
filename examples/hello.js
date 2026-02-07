// ============================================================
// rawjs - Feature Showcase
// ============================================================

// --------------------------------------------------
// 1. Basic Hello World
// --------------------------------------------------
console.log("Hello, World!");

// --------------------------------------------------
// 2. Variables & Arithmetic
// --------------------------------------------------
let x = 10;
let y = 20;
console.log("x + y =", x + y);
console.log("x * y =", x * y);
console.log("x ** 3 =", x ** 3);

const PI = 3.14159;
console.log("PI =", PI);

// --------------------------------------------------
// 3. String Operations
// --------------------------------------------------
let greeting = "Hello";
console.log(greeting.length);
console.log(greeting.toUpperCase());
console.log(greeting.slice(1, 3));
console.log(greeting.includes("ell"));
console.log(greeting.repeat(3));
console.log(greeting.padStart(10, "*"));
console.log("  spaces  ".trim());
console.log("a-b-c".split("-"));

// --------------------------------------------------
// 4. Template Literals
// --------------------------------------------------
let who = "World";
console.log(`Hello, ${who}!`);
console.log(`2 + 3 = ${2 + 3}`);
console.log(`nested: ${"<" + who + ">"}`);

// --------------------------------------------------
// 5. Functions & Arrow Functions
// --------------------------------------------------
function add(a, b) {
  return a + b;
}
console.log("add(3, 4) =", add(3, 4));

const multiply = (a, b) => a * b;
console.log("multiply(5, 6) =", multiply(5, 6));

// Default parameters
function greetPerson(name, prefix) {
  if (prefix === undefined) {
    prefix = "Hello";
  }
  return `${prefix}, ${name}!`;
}
console.log(greetPerson("Alice"));
console.log(greetPerson("Bob", "Hi"));

// Rest parameters
function sum() {
  let total = 0;
  for (let i = 0; i < arguments.length; i++) {
    total = total + arguments[i];
  }
  return total;
}

// --------------------------------------------------
// 6. Objects
// --------------------------------------------------
let person = {
  name: "Alice",
  age: 30,
  greet: function () {
    return "Hi, I'm " + this.name;
  },
};
console.log(person.greet());
console.log("name" in person);
console.log(Object.keys(person));

// Computed property access
let key = "age";
console.log(person[key]);

// Object.keys
let personKeys = Object.keys(person);
console.log("keys count:", personKeys.length);

// --------------------------------------------------
// 7. Arrays & Array Methods
// --------------------------------------------------
let arr = [1, 2, 3, 4, 5];
console.log("length:", arr.length);
console.log("includes(3):", arr.includes(3));
console.log("indexOf(4):", arr.indexOf(4));
console.log("slice(1,3):", arr.slice(1, 3));
console.log("join('-'):", arr.join("-"));

let arr2 = [10, 20];
arr2.push(30);
console.log("after push:", arr2);
arr2.pop();
console.log("after pop:", arr2);
arr2.unshift(0);
console.log("after unshift:", arr2);
arr2.shift();
console.log("after shift:", arr2);

console.log("concat:", [1, 2].concat([3, 4]));
console.log("reverse:", [5, 4, 3, 2, 1].reverse());

// --------------------------------------------------
// 8. Control Flow
// --------------------------------------------------

// for loop
for (let i = 0; i < 5; i++) {
  if (i % 2 === 0) {
    console.log(i + " is even");
  }
}

// while loop
let w = 0;
while (w < 3) {
  console.log("while:", w);
  w++;
}

// do-while loop
let d = 0;
do {
  console.log("do-while:", d);
  d++;
} while (d < 3);

// switch statement
function dayName(n) {
  switch (n) {
    case 0:
      return "Sunday";
    case 1:
      return "Monday";
    case 2:
      return "Tuesday";
    case 3:
      return "Wednesday";
    case 4:
      return "Thursday";
    case 5:
      return "Friday";
    case 6:
      return "Saturday";
    default:
      return "Unknown";
  }
}
console.log(dayName(0));
console.log(dayName(3));
console.log(dayName(6));
console.log(dayName(99));

// Ternary / conditional expression
let val = 42;
console.log(val > 0 ? "positive" : "non-positive");

// --------------------------------------------------
// 9. Closures & Higher-order Functions
// --------------------------------------------------
function counter() {
  let count = 0;
  return function () {
    count++;
    return count;
  };
}

let c = counter();
console.log("counter:", c(), c(), c());

function makeAdder(n) {
  return function (x) {
    return x + n;
  };
}
let add5 = makeAdder(5);
console.log("add5(10) =", add5(10));
console.log("add5(20) =", add5(20));

// --------------------------------------------------
// 10. Error Handling (try/catch/finally)
// --------------------------------------------------
try {
  throw new Error("test error");
} catch (e) {
  console.log("Caught:", e.message);
}

try {
  throw new TypeError("something broke");
} catch (e) {
  console.log("Caught:", e.message);
} finally {
  console.log("Finally always runs");
}

function safeDiv(a, b) {
  if (b === 0) {
    throw new RangeError("Division by zero");
  }
  return a / b;
}

try {
  console.log(safeDiv(10, 2));
  console.log(safeDiv(10, 0));
} catch (e) {
  console.log("Caught:", e.message);
}

// --------------------------------------------------
// 11. for-in (Object enumeration)
// --------------------------------------------------
let car = { make: "Toyota", model: "Corolla", year: 2024 };
for (let prop in car) {
  console.log("for-in:", prop, "=", car[prop]);
}

// --------------------------------------------------
// 12. for-of (Iterable iteration)
// --------------------------------------------------
let colors = ["red", "green", "blue"];
for (let color of colors) {
  console.log("for-of:", color);
}

// for-of with strings
for (let ch of "abc") {
  console.log("char:", ch);
}

// --------------------------------------------------
// 13. Symbol
// --------------------------------------------------
let s1 = Symbol("desc");
let s2 = Symbol("desc");
console.log("Symbol typeof:", typeof s1);
console.log("Symbols equal?", s1 === s2);
console.log("Symbol.iterator:", typeof Symbol.iterator);

// Using symbols as property keys
let myKey = Symbol("myKey");
let obj = {};
obj[myKey] = "secret value";
console.log("symbol prop:", obj[myKey]);

// --------------------------------------------------
// 14. Map
// --------------------------------------------------
let map = Map();
map.set("name", "Alice");
map.set("age", 30);
map.set(42, "the answer");

console.log("Map size:", map.size);
console.log("Map get('name'):", map.get("name"));
console.log("Map has('age'):", map.has("age"));
console.log("Map has('missing'):", map.has("missing"));

map.delete("age");
console.log("After delete, size:", map.size);

let map2 = Map();
map2.set("a", 1);
map2.set("b", 2);
map2.set("c", 3);
console.log("Map keys:", map2.keys());
console.log("Map values:", map2.values());
console.log("Map entries:", map2.entries());

// Map with NaN key (SameValueZero)
let nanMap = Map();
nanMap.set(NaN, "not a number");
console.log("NaN key:", nanMap.get(NaN));

// --------------------------------------------------
// 15. Set
// --------------------------------------------------
let set = Set();
set.add(1);
set.add(2);
set.add(3);
set.add(2); // duplicate - ignored

console.log("Set size:", set.size);
console.log("Set has(2):", set.has(2));
console.log("Set has(99):", set.has(99));

set.delete(2);
console.log("After delete, size:", set.size);

let set2 = Set();
set2.add("x");
set2.add("y");
set2.add("z");
console.log("Set values:", set2.values());

// --------------------------------------------------
// 16. Math Object
// --------------------------------------------------
console.log("Math.PI:", Math.PI);
console.log("Math.E:", Math.E);
console.log("Math.abs(-5):", Math.abs(-5));
console.log("Math.floor(3.7):", Math.floor(3.7));
console.log("Math.ceil(3.2):", Math.ceil(3.2));
console.log("Math.round(3.5):", Math.round(3.5));
console.log("Math.sqrt(16):", Math.sqrt(16));
console.log("Math.pow(2, 10):", Math.pow(2, 10));
console.log("Math.min(3, 1, 2):", Math.min(3, 1, 2));
console.log("Math.max(3, 1, 2):", Math.max(3, 1, 2));

// --------------------------------------------------
// 17. JSON
// --------------------------------------------------
let data = { name: "Alice", scores: [95, 87, 92] };
let json = JSON.stringify(data);
console.log("JSON.stringify:", json);
let parsed = JSON.parse(json);
console.log("JSON.parse:", parsed.name, parsed.scores);

// --------------------------------------------------
// 18. typeof Operator
// --------------------------------------------------
console.log("typeof 42:", typeof 42);
console.log("typeof 'hello':", typeof "hello");
console.log("typeof true:", typeof true);
console.log("typeof undefined:", typeof undefined);
console.log("typeof null:", typeof null);
console.log("typeof {}:", typeof {});
console.log("typeof []:", typeof []);
console.log("typeof function:", typeof add);

// --------------------------------------------------
// 19. Bitwise Operations
// --------------------------------------------------
console.log("5 & 3 =", 5 & 3);
console.log("5 | 3 =", 5 | 3);
console.log("5 ^ 3 =", 5 ^ 3);
console.log("~5 =", ~5);
console.log("5 << 1 =", 5 << 1);
console.log("20 >> 2 =", 20 >> 2);

// --------------------------------------------------
// 20. Global Functions
// --------------------------------------------------
console.log("parseInt('42') =", parseInt("42"));
console.log("parseInt('0xFF', 16) =", parseInt("0xFF", 16));
console.log("parseFloat('3.14') =", parseFloat("3.14"));
console.log("isNaN(NaN):", isNaN(NaN));
console.log("isNaN(42):", isNaN(42));
console.log("isFinite(42):", isFinite(42));
console.log("isFinite(Infinity):", isFinite(Infinity));

console.log("--- All tests passed! ---");
