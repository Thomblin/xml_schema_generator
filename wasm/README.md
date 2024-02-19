# Web Assembly Integration

## Compilation

xml_schema_generator can be included in a web page using wasm

        cd wasm
        wasm-pack build
        cd www
        npm install
        npm run start

        open http://localhost:8080/ in your favourite browser

In case of the error `Error: error:0308010C:digital envelope routines::unsupported`

run `export NODE_OPTIONS=--openssl-legacy-provider`

@see https://github.com/webpack/webpack/issues/14532
@see https://stackoverflow.com/questions/69692842/error-message-error0308010cdigital-envelope-routinesunsupported

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