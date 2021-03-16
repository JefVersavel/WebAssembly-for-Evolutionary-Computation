const { Console } = require('console');
const fs = require('fs')
var source = fs.readFileSync('./p5.wasm')

const typedArray = new Uint8Array(source);
const param0 = new WebAssembly.Global({
    value: 'f64',
    mutable: false
}, 0);
const param1 = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 0);
const param2 = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 0);
const param3 = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 0);
const param4 = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 0);

const state = new WebAssembly.Global({
    value: 'f64',
    mutable: true
}, 21)

let importObject = {
    external: { param0, param1, param2, param3, param4 },
    internal: { state }
};
WebAssembly.instantiate(typedArray, importObject).then(result => {
    let json = {}
    json.outcome = result.instance.exports.main()
    json.internal = result.instance.exports.state.value
    console.log(json)

}).catch(e => {
    console.log(e)
});
