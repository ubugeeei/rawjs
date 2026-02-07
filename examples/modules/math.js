// ============================================================
// rawjs - ESM Module: math utilities
// ============================================================

// Named exports: functions
export function add(a, b) {
  return a + b;
}

export function multiply(a, b) {
  return a * b;
}

export function square(x) {
  return x * x;
}

// Named exports: constants
export const PI = 3.14159265;
export const E = 2.71828182;

// Default export
export default function subtract(a, b) {
  return a - b;
}
