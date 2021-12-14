# Symbol Structural Editor

## Advantages and Disadvantages

### Advantages

- code style guides are a thing of the past! mostly. we can focus more now on
  content than minor things like indentation, spaces after commas, and other
  irrelevant details
- working directly on AST means that source-code is a lot smaller, and
  compile times would be a lot faster (and compilers could be a lot easier to
  write, when everyone isn't worrying about parsing. of course that effort
  would probably be directed into the renderer, but still seems like it could
  be a win)
- syntax errors no more!
- can also help with type errors
- just less typing in general
- you can make different renderers! like lisp? i like you, and you get a
  lispy renderer. How about C? A classic, of course we have that. Accoustomed
  to reading javascript? Or perhaps python is your cup of tea. Whatever you
  like, we can devise a renderer to make most any language look like home, so
  you can stop worrying about syntax and start learning the meat of a
  language.
- take advantage of modern pretty-printing, so theres no more fiddling with
  layout. Dynamically format your code to fit the screen so long lines split
  in a nice way in a narrow window, but expand to fill empty space when
  running fullscreen (if that is your cup of tea)
- spaces in the middle of variable names? and other bizarre things you can do
- explorability. when you search for things/learn controls, you get to see all
  the strange things your language will let you do. when you are worried about
  producing syntactically correct and typed code, you don't spend much time
  exploring. when every edit is just a hotkey and you know that every edit will
  leave your code correct, there is a lot more room for exploration
  (potentially, we will have to see how this works in general)

### Disadvantages
- copy-pasting code doesn't make sense. you could of course make a parser to
  import text files to a usable AST, but that sorta defeats the purpose of
  avoiding writing a parser in the first place, doesn't it? and having flexible
  renderers and all that?
- probably more effort to build an editor with the language than to just build
  the language normally. especially since tooling already exists in the world of
  language development
- tight coupling between a language and it's editor. sort of the logical
  conclusion of the direction IDEs have been going in for a while, but still not
  wonderful
- formatting is less flexible. in some ways this is a good thing; really BAD
  formatting can't be done. but by the same stroke, some useful formatting won't
  be accsesable either.


## Application Architecture

We have 3 parts:

### application backend
- currently brick for TUI
- someday maybe will have proper GUI

### language
- Simply Typed Lambda Calculus

### core library
- all the tree stuff

language depends on core. application depends on core. langauge and backend have
no interdependence (nor even knowledge of each other). work to split files
appropriately

### Other architecture notes

1. Structured datatype for type checking and validating the tree, unstructured
   for main application. Sometimes less is more when it comes to types (tho i do
   wonder if we could come up with a typeclass for this, with some interface
   like select :: Int -> A -> Maybe A for selecting the subtree that would
   support most of our tree like functionality, but might be more robust?)
2. Pass functions around as an element of state (functions answer question: how
   do we react to new input?)
3. Transactional system to support more sophisticated editing
4. Stack machine for rendering a tree to text (library already supported it, but
   I thought it was a neat idea. I guess the library developers also thought it
   was a good thing)
5. Duplicating Brick interface to reduce coupling between applications

## Todo
- make binary operators prettier: remove unecessary parens based on operator
  precedence
- scrolling
- more advanced wrapping behavior
- alternative renderers: algol, lisp, python, prose, unreadable
- select wrapping expression/value or something
- swap between renderers
- status bar? show info about location under cursor, current renderer?
- comments? could be useful for annotating code. can be a sort of statement.
  could also be useful for refactoring; e.g. when a change must mess with
  code in other portions of the program, commenting out the affected region
  could provide hints on how to fix things up again.
- arrow key navigation (might be dependent on prettyprint/screen width, so
  this could be a bit complex)
- undo/redo
- cut/paste
- suspend type checker on portions of the code (potentially useful for
  refactoring)

certain things are defined in such a way that there is only 1 possible type
that could occupy the hole, so it doesn't make sense to have a hole in the
first place. i.e. with the assignment, the first thing MUST be a declaration.
nothing else makes sense. same thing goes for aything with a list in it. the
list of function args MUST exist and be a list

### User interface considerations

So i think the ideal interface would consist of a couple of parts:

1. modifying values would consist of a dropdown autocomplete sort of thing.
   You'd start typing, which would restrict the number of possible completions
   that could be inserted. This completion list would only consist of things
   that would allow the program to work if inserted, so e.g. correctly typed and
   in scope. There would also be an allowance for values with literals that
   don't come from a finite set, so while e.g. boolean values don't need
   anything special, strings, lists, and numbers need a way for manual input to
   happen. Also, just a reminder that the autocomplete should probably not
   search for anything if the user enters a character that would make the
   currently entered string not match anything.
2. navigating the code. this is mostly ready right now, but could be streamlined

Once those are in place, the next big step is to consider how well refactoring
could be done. This will involve copy-pasting probably, in addition to some
other stuff.

