# Hell

A shell (again...) written in Haskell.

Thanks to the superior language it's the better version of [kjell](https://github.com/sebastianselander/kjell).

# Implemented
- `;`-list
- `&&`-list
- `||`-list
- Pipes
- Builtins (cd, exit, pwd)
- External commands
- Negation (`!`)
- Redirection

## Notes
Redirections do not exactly parse according to the posix standard.
`cat a1<file` is parsed as `{cat a} 1< {file}` in bash/zsh etc.\
In hell `cat a1<file` is parsed as `{cat a1} < {file}`.

# TODO
- Following [build-your-own-shell](https://github.com/tokenrove/build-your-own-shell/tree/master) for what to implement (currently at stage 2, act 1).

# Implementation todos
- Perhaps an effect system to better encapsulate IO effects
