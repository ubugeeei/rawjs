// === Async/Await examples ===

// --- 1. Basic async function ---
async function greet() {
  return "hello async!";
}

let p1 = greet();
p1.then(function(val) {
  console.log("1. Basic async:", val);
});

// --- 2. Await Promise.resolve ---
async function awaitResolved() {
  let val = await Promise.resolve(42);
  return val;
}

awaitResolved().then(function(val) {
  console.log("2. Await resolved:", val);
});

// --- 3. Await non-promise value ---
async function awaitValue() {
  let val = await 100;
  return val + 1;
}

awaitValue().then(function(val) {
  console.log("3. Await non-promise:", val);
});

// --- 4. Multiple awaits ---
async function multipleAwaits() {
  let a = await Promise.resolve(10);
  let b = await Promise.resolve(20);
  let c = await Promise.resolve(30);
  return a + b + c;
}

multipleAwaits().then(function(val) {
  console.log("4. Multiple awaits:", val);
});

// --- 5. Async with rejected promise ---
async function awaitRejected() {
  try {
    let val = await Promise.reject("oops");
    return val;
  } catch (e) {
    return "caught: " + e;
  }
}

awaitRejected().then(function(val) {
  console.log("5. Await rejected:", val);
});

// --- 6. Async throw becomes rejection ---
async function throwsError() {
  throw "async error";
}

throwsError().catch(function(err) {
  console.log("6. Async throw:", err);
});

// --- 7. Await chaining ---
async function getNumber() {
  return 5;
}

async function doubleIt() {
  let n = await getNumber();
  return n * 2;
}

doubleIt().then(function(val) {
  console.log("7. Await chaining:", val);
});
