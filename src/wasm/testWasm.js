const fs = require('fs')
var source = fs.readFileSync('./p4.wasm')

const typedArray = new Uint8Array(source);
const param0 = new WebAssembly.Global({
    value: 'f64',
    mutable: false
}, 0);
const param1 = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 5);
const param2 = param1
const param3 = param1
const param4 = param1

const state = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 21)

let importObject = {
    external: { param0, param1, param2, param3, param4 },
    internal: { state }
};
WebAssembly.instantiate(typedArray, importObject).then(result => {
    console.log(result.instance.exports.main());
    console.log(result.instance.exports.state.value)
}).catch(e => {
    console.log(e)
});
