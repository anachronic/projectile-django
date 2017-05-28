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

## Key bindings

All keys described here run under the prefix for the keymap, which you
should set yourself by having this in your config:

``` emacs-lisp
(global-set-key (kbd "M-m d") 'projectile-django-map)
```

Of course, `M-m d` are keys that i chose myself. You can set whatever
prefix you want.

`<prefix> + s`: Run the server<br>
`<prefix> + m`: Migrate all applications<br>
`<prefix> + l`: Load a fixture using `loaddata`<br>
`<prefix> + t`: Run all tests<br>
`<prefix> + v`: Visit project's web page<br>

`<prefix> + j t`: Jump to template for current view<br>
`<prefix> + j v`: Jump to view for current template


## Running the server

Call it with `<prefix> s` or `M-x projectile-django-server`.

The server opens in a separate buffer in
`projectile-django-server-mode` which derives from `comint-mode`. Any
subsequent calls to `projectile-django-server` will switch to the
server buffer if it's already running.

Inside the server buffer you can kill the server by calling
`projectile-django-terminate-server` or by just hitting `C-c C-k`. You
can also restart the server by calling
`projectile-django-restart-server` or hitting `C-c C-r` in the server
buffer. Lastly, you can bury the buffer by hitting `C-c C-q`.


The server buffer is known to work with `ipdb` as long as you set up
your ansi filter. It is advised to
have [xterm-color](https://github.com/atomontage/xterm-color)
installed.


## Running migrations

Call `M-x projectile-django-migrate-all` or `<prefix> m`. Every time
you call this, the buffer will reset. Hitting `q` inside this buffer
will quit the buffer.

## Running tests

Call `M-x projectile-django-test-all` or `<prefix> t`. It has the same
behavior as migrations. Every time you call this, tests will
reset. Quitting will trigger the quit action.

## Loading fixtures

Call `M-x projectile-django-loaddata` or `<prefix> l`. It will ask for
what file you want to load. This searches every fixture directory and
collects every fixture there. When fixtures get loaded, use `q` to
quit the buffer.

## Visiting the page

Call `M-x projectile-django-visit-page` or `<prefix> v`. You will
visit your page with the default browse. Projectile-django will use
the `browse-url` function to open the page.

## Jumping

There are, for the moment, only two jumping options.

* From view to template
* From template to view

### Jump to template from view

`M-x projectile-django-jump-to-template` or `<prefix> j t`. This
searches for any template string in the document and jumps directly to
it. This **does not** infer templates based on inheritance or
anything. It's pretty dumb.

### Jump to view from template

`M-x projectile-django-jump-to-view` or `<prefix> j v`. This command
**requires** `ag` to run. It will search the project for the desired
template filename and jump to it or prompt for file depending on the
options gathered.


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

-   Support `makemigrations` app-wise
-   Support `migrate` app-wise
-   Jump to settings
-   Jump to urls

Suggestions are also welcome.
