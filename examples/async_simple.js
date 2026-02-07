// Async with non-promise await
async function withAwait() {
  let val = await 42;
  return val;
}

withAwait().then(function(val) {
  console.log("result:", val);
});
