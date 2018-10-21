const { readFileSync } = require("fs");

const run = async () => {
    const buffer = readFileSync("./out.wasm");
    const module = await WebAssembly.compile(buffer);
    const instance = await WebAssembly.instantiate(module);
    console.log(instance.exports._main());
}

run();
