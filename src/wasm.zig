// Simple WASM module with basic arithmetic functions

// Export a simple function that can be called from JS
export fn add(a: i32, b: i32) i32 {
    return a + b;
}

// Export a multiplication function
export fn multiply(a: i32, b: i32) i32 {
    return a * b;
}
