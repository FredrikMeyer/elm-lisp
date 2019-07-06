# Lisp in Elm

This is a Lisp interpreter written in Elm.

I try to follow the instructions at [Mal](https://github.com/kanaka/mal/blob/master/process/guide.md).


- [x] Step 0: REPL
- [x] Step 1: Read and print 
- [x] Step 2: Eval
- [x] Step 3: Environments (implemented `let` instead of `let*`)
- [x] Step 4: If fn do (mostly)
- [ ] Step 5: Tail call optimization
- [ ] Step 6: Files, mutation, and evil
- [ ] Step 7: Quoting
- [ ] Step 8: Macros 
- [ ] Step 9: Try 

## Run in browser

```
parcel src/static/index.html
```

## Build

```
parcel build src/static/index.html --out-dir docs --public-url ./
```

## Run tests

```
elm-test --watch
```
