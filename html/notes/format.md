# I hate 'display'

WIP WIP WIP

(format "template" arg ...) ;sprintf
(format obj) ;like write but returns string 
(format! "template" arg ...) ;printf 
(format! arg) ;write

The ones with a string template are special forms and process the
template string at compile time and ensure correct number of args.

Need a way to let a special form's name also appear as an identifier
like Guile does it with record accessors and shit.
