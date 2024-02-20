# Web Assembly Integration

## Compilation

xml_schema_generator can be included in a web page using wasm

        cd wasm
        wasm-pack build --target web

run a local server for testing. For example using python:

        python3 -m http.server 12345 --bind 127.0.0.1

open http://127.0.0.1:12345/ in your favourite browser

## Optimization

        wasm-pack build --release

        # Optimize for size.
        wasm-opt -Os -o output.wasm input.wasm

        # Optimize aggressively for size.
        wasm-opt -Oz -o output.wasm input.wasm

        # Optimize for speed.
        wasm-opt -O -o output.wasm input.wasm

        # Optimize aggressively for speed.
        wasm-opt -O3 -o output.wasm input.wasm

## Github Pages

        wasm-pack build --target web

        copy pkg folder to `https://github.com/Thomblin/thomblin.github.io/tree/main/xml_schema_generator`