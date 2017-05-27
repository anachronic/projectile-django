# projectile-django

`projectile-django` is (for the moment) a collection of functions
dedicated to help in the django development process. It, of course,
requires `projectile` and `comint`.

The main reason for the existance of this project is the outdated
nature of other django projects and their failure to provide a
consistant workflow.

The main entry point is given by the `projectile-django-map`
keymap. The main functions now are migrations and running a
server. Every function call to this project **must** be from a
projectile-active buffer. This also means you can have multiple django
projects running at once and `projectile-django` should support it.


## Running the server

Call it with `<projectile-prefix> s`. It will start the server in
`comint-mode`.

You can restart the server by calling
`projectile-django-terminate-server` or by just hitting `C-c C-k` in
the server buffer.

You can also restart the server by calling
`projectile-django-restart-server` or hitting `C-c C-r` in the server
buffer.

Lastly, you can bury the buffer by hitting `C-c C-q` on the server buffer


## Running migrations

Call `projectile-django-migrate-all`. Every time you call this, the
buffer will reset. Hitting `q` inside this buffer will quit the buffer.


## Quitting

This project will continue to grow as time passes, and there will be
many buffers created by it.

The variable `projectile-django-default-quit-action` controls what
should happen when you *quit* a buffer. Default is `'bury` which just
means bury the buffer. It can also be set to `'kill` in order to kill
the buffer. Any other value other than those two will default to
`'bury`.


## Variables

A short description of the variables that `projectile-django` sets.


<a id="org8c2a2e3"></a>

### `projectile-django-serve-for-everyone`

When set to `t`, binds the server to the address `0.0.0.0` instead of
localhost or 127.0.0.1.


### `projectile-django-default-port`

This is a **number** that defines the port on which the django server
will be run.


### `projectile-django-python-interpreter`

Full path to the python interpreter that should be used. **This
variable is global**. You are advised to use a different package to
manage virtual environments.

If you really want to use this variable, it's best to use
project-local variables.



### `projectile-django-default-quit-action`

Controls whether to bury or kill the current buffer when quitting. Set
to `'kill` for killing buffer or `'bury` to bury them. Any other value
will default to `'bury`, which is also the preset value.




### `projectile-django-server-command`

Default is `runserver`. Change it to run another command for the
server. Maybe there's plugins or something. I don't really know.




## Extending

Pull requests are always welcome.

The intended functionality for this project as of right now is also:

-   Support `loaddata` for fixtures
-   Support `makemigrations` app-wise
-   Support `migrate` app-wise
-   Support jumping to template (possibly using `ag`)
-   Support jumping to view
-   Jump to settings
-   Jump to urls

Suggestions are also welcome.