// === using Declaration Examples ===

// --- 1. Basic using with Symbol.dispose ---
{
  let disposed = false;
  let resource = {};
  resource[Symbol.dispose] = function() {
    disposed = true;
    console.log("1. Resource disposed");
  };

  using r = resource;
  console.log("1. Using resource");
}
// After block, dispose should have been called

// --- 2. Multiple using declarations (LIFO order) ---
{
  using a = {
    [Symbol.dispose]() {
      console.log("2. Disposed A (should be second)");
    }
  };

  using b = {
    [Symbol.dispose]() {
      console.log("2. Disposed B (should be first)");
    }
  };

  console.log("2. Using A and B");
}

// --- 3. null resource (no-op) ---
{
  using r = null;
  console.log("3. null resource - no error");
}

// --- 4. undefined resource (no-op) ---
{
  using r = undefined;
  console.log("4. undefined resource - no error");
}
