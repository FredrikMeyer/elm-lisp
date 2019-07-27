# Lisp in Elm

This is a Lisp interpreter written in Elm. Test it live on [fredrikmeyer.github.io/elm-lisp/](https://fredrikmeyer.github.io/elm-lisp/).

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
npm start
```

## Build

```
npm run build
```

## Run tests

```
npm test
```

Or
```
elm-test --watch
```
for live updates.

# TODOs

 - [ ] Use [CodeMirror](https://codemirror.net/doc/manual.html) to get a better editor. I.e. don't do any HTML in Elm.
