// ============================================================
// rawjs - ESM Module: utility functions
// ============================================================

export function capitalize(str) {
  if (str.length === 0) return str;
  return str.slice(0, 1).toUpperCase() + str.slice(1);
}

export function repeat(str, n) {
  let result = "";
  for (let i = 0; i < n; i++) {
    result = result + str;
  }
  return result;
}

export function range(start, end) {
  let arr = [];
  for (let i = start; i < end; i++) {
    arr.push(i);
  }
  return arr;
}
