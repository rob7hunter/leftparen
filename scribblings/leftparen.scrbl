#lang scribble/doc
@(require scribble/manual)

@title{LeftParen 0.3 Documentation}

Website: @link["http://leftparen.com"]{http://leftparen.com}

LeftParen is a framework for quickly creating web apps.  It runs on
PLT Scheme v4.1 or greater.  LeftParen is released under an @link["http://github.com/vegashacker/leftparen/tree/master/MIT-LICENSE.txt"]{MIT License}.  The source is available on @link["http://github.com/vegashacker/leftparen/tree/master"]{github}.

@section{Installing LeftParen}

You'll need PLT Scheme v4.1 or greater installed.

Make sure that @scheme[mzscheme] is in your path.  You should be ready
to go if you can do this:

@verbatim{
% mzscheme
Welcome to MzScheme v4.1...
> 
}

Installing LeftParen is done behind the scenes with a @link["http://planet.plt-scheme.org/"]{PLaneT} @scheme[require].  See @secref{tutorials} for an example of this.  When you
first issue one of these @scheme[require] commands, you'll automatically download the LeftParen files to your local PLaneT cache.  This can sometimes take a few moments, so be prepared to wait a bit.

@section[#:tag "tutorials"]{Tutorials}

@subsection{Hello, World}

We're going to make a project called @tt{hello-world}.  Change to the directory that you'd like to make the project in.  Then issue

@verbatim{% mzscheme -e '(require (planet "bootstrap.scm" ("vegashacker" "leftparen.plt" 2 2)))' project hello-world}

This will create a @tt{hello-world} project directory for you.  In this directory you'll find the @tt{script} directory, which contains some useful scripts.  All paths are relative to this project directory, so when calling scripts, you always want to be at the project root.

@verbatim{% cd hello-world}

We need to make the scripts executable:

@verbatim{% chmod u+x script/*}

LeftParen has automatically generated everything we need to run our web app---we just need to start the server (again, you should be at the project root directory):

@verbatim{
% ./script/server
Web server started on port 8765
Listening on IP address: 127.0.0.1
Type stop to stop the server and exit
Type restart to restart the server
}

Point your browser to @link["http://localhost:8765"]{http://localhost:8765} and you should see a familiar greeting:

@tt{Hello, World!}

@subsection{Blogerton the Blog}

Now let's try implementing the true "hello world" of web apps---a blog.  First, execute the following commands from the directory in which you want to create your project directory:

@verbatim{
% mzscheme -e '(require (planet "bootstrap.scm" ("vegashacker" "leftparen.plt" 2 2)))' project blogerton
% cd blogerton
% chmod u+x script/*
}

@subsubsection{Changes to @tt{app.scm}}

We need to register a couple of pages in our app.  The @scheme[index-page] was already set up for you, but you'll need to add a page to create new posts, and one to view them.  Make the @scheme[define-app] call look like this:

@schemeblock[
(define-app my-app
  (index-page (url "/"))
  (create-post-page (url "/post"))
  (view-post-page (url "/view/" (string-arg))))
]

@subsubsection{Changes to @tt{main.scm}}

Now we need to define those pages that we declared in @tt{app.scm}.

@schemeblock[
(define-page (index-page req)
  (** `(h1 "Blogerton")
      `(p ,(web-link "Create a new post" (page-url create-post-page)))
      `(ul ,@(map (lambda (p) `(li ,(paint-blog-post p)))
                  (load-where '((type . blog-post))
                              #:sort-by 'created-at #:compare >)))))

(define-page (create-post-page req)
  (form '((title "Title" text) (body "Body" long-text))
        #:init '((type . blog-post))
        #:on-done (lambda (post) (redirect-to-page view-post-page (rec-id post)))))

(define-page (view-post-page req post-id)
  (paint-blog-post (load-rec post-id #:ensure '((type . blog-post)))))

(define (paint-blog-post post)
  `(div (h2 ,(rec-prop post 'title))
        (p ,(rec-prop post 'body))))
]

@subsubsection{Launch Blogerton}

You're ready for launch.  Start the server with

@verbatim{% ./script/server}

and you should have a basic blogging app, with persistent data, in 19 lines of code.

@section{More Documentation to Come}

We need to get a full LeftParen reference up (not just simple tutorials).  There's lots more cool stuff in LeftParen that this document doesn't yet address.

@section{About/Acknowledgements}

LeftParen was written by @link["http://robhunter.org"]{Rob Hunter}, but it builds heavily on (and, in fact, often directly incorporates) the work of @link["http://untyped.com/"]{Untyped} (@link["http://planet.plt-scheme.org/display.ss?package=instaservlet.plt&owner=untyped"]{instaservlet} and @link["http://planet.plt-scheme.org/display.ss?package=dispatch.plt&owner=untyped"]{dispatch}), @link["http://scheme.dk/blog/"]{Jens Axel Soegaard} (@link["http://planet.plt-scheme.org/display.ss?package=web.plt&owner=soegaard"]{web.plt}), and of course, @link["http://www.plt-scheme.org/"]{PLT Scheme}.
