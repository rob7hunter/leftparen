#lang scribble/doc
@(require scribble/manual)

@title{LeftParen 0.4 Documentation}

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

@verbatim{% mzscheme -e '(require (planet "bootstrap.scm" ("vegashacker" "leftparen.plt" 3 0)))' project hello-world}

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
% mzscheme -e '(require (planet "bootstrap.scm" ("vegashacker" "leftparen.plt" 3 0)))' project blogerton
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

@section{Reference}

@subsection{Forms}

Most web applications make some use of web forms.  The @scheme[form] function lets easily you get and process input from your users.

@defproc[(form (field-specs (listof field-spec?))) xexpr?]
@defthing[field-spec? (list symbol? string? field-type?)]
@defthing[field-type? (or/c 'text 'long-text 'number 'password 'image 'checkbox
		            'radio 'drop-down)]

You create a form by listing, in order, "field specifications".  For example, you might want a title field, followed by a description text box, followed by a photo upload field.  Note that the many keyword arguments available to the @scheme[form] function aren't documented yet.

Each field spec is of the form @scheme[(field-name label field-type)].  For example, you create a title field, you might use the spec @scheme[(title "Enter a title" text)].  The entire example metioned above might look like this:

@schemeblock[
(form '((title "Title" text) 
        (description "Description" long-text)
        (photo "Your photo" image)))
]


@subsubsection{Image uploads}

By default, uploaded images are stored in the @tt{uploaded-files} directory in your project directory.  You can customize this with the @scheme[*PATH_TO_UPLOADED_FILES*] setting.  When images are saved, their original filenames are used with a 5-character code pre-pended to make filenames unique.

@subsection{Sessions}

A session is an object that allows you to easily store state about individual visitors to your web app.  Sessions are stored on the server as a record with a virtually impossible-to-guess id.  A cookie is left in the user's web browser, which contains a pointer to a particular session id.  These cookies expire one month after creation and, currently, this can't be changed.

@subsubsection{Creating sessions}

@defform[(define-session-page
           (page-name request-iden session-iden page-args ...) 
	   body ...)]

This is an alternate to @scheme[define-page], most commonly used in @scheme[main.scm].  The only difference is that after the request identifier, you must provide a session identifier.  For example, to keep a counter (unique to each user), you could write:

@schemeblock[
(define-session-page (foo-page req sesh)
  (let ((c (session-get-val sesh 'counter 0)))     
    (session-put-val! sesh 'counter (+ 1 c))
    (number->string c)))
]

When you define a session page, the session is automatically fetched for you (and created if necessary), and bound to the session identifier you provided.

@subsubsection{Accessing sessions}

@defproc[(session-get-val (session session) (key symbol) (missing-val any #f)) any]

@defproc[(session-put-val! (session session) (key symbol) (val any)) session]


@subsection{Users}

LeftParen provides built-in functionality for dealing with users, including registering users, logging users in and out, and storing persistent data about users.  To get up-and-running quickly, you can use the high-level @scheme[welcome-message] function:

@defproc[(welcome-message (session session?) (#:on-success success-fn (or/c (-> user? xexpr?) #f) #f) (#:no-register no-register boolean? #f)) xexpr]{The function @scheme[welcome-message] produces a small area of text and links (commonly found in the top-right area of a web app).  If the user is not currently logged in, login and register links are presented.  If the user is logged in, a message welcoming them is displayed, along with a link to log out.}

@defproc[(current-user (session session?)) (or/c user? #f)]{The function @scheme[current-user] returns the current user record, or @scheme[#f] if no user is available in the current session.}

As an example, here is the complete page code for a web app that allows users to register, login and logout, and which prints a secret message if the user is logged in:

@schemeblock[
(define-session-page (index-page req sesh)
  (** (welcome-message sesh)
      (aif (current-user sesh)
           (format "The secret, ~A, is 42." (rec-prop it 'username))
           "No secret for you.")))
]

@subsection{Feeds}

You can create Atom or RSS feeds in your web app.  A feed in LeftParen is just a page crafted in a paricular way.  The core functions involved are @scheme[atom-feed] and @scheme[rss-feed]:

@defproc[(atom-feed (atom-feed-page page?)
		    (#:feed-title feed-title string?)
		    (#:feed-updated/epoch-seconds updated-seconds integer?)
		    (#:author-name author-name string?)
		    (#:feed-description feed-description (or/c #f string?) #f)
		    (#:feed-id feed-id string? THE_URL_OF_THE_GIVEN_ATOM_FEED_PAGE)
		    (#:related-content-link related-content-link string? THE_LINK_TO_YOUR_WEB_APP)
		    (#:items atom-items (list-of atom-item?) '()))
                    response/full?]

@defproc[(rss-feed (rss-feed-page page?)
		   (#:feed-title feed-title string?)
		   (#:feed-description feed-description string?)
		   (#:related-content-link related-content-link string? THE_LINK_TO_YOUR_WEB_APP)
		   (#:items rss-items (list-of rss-item?) '()))
                   response/full?]

The @scheme[#:items] argument in each of these functions is a list of items constructed with @scheme[atom-item] and @scheme[rss-item]:

@defproc[(atom-item (#:title title string?)
		    (#:url url string?)
		    (#:updated-epoch-seconds updated-seconds integer?)
		    (#:content content (or/c #f string?) #f))
		    atom-item?]

@defproc[(rss-item (#:title title string?)
		   (#:url url string?)
		   (#:content content (or/c #f string?) #f))
		   rss-item?]

Here's an example Atom feed page:

@schemeblock[
(define-page (article-feed-page req)
  #:blank #t
  (atom-feed article-feed-page
             #:feed-title "LeftParen blog"
             #:feed-description "On LeftParen..."
             #:feed-updated/epoch-seconds (current-seconds)
             #:author-name "LP staffers"
             #:items (list
                      (atom-item #:title "Status update..."
                                 #:url "http://blog.../50308696"
                                 #:updated-epoch-seconds
				 (current-seconds)
                                 #:content "I’m nearing a...")
                      (atom-item #:title "LeftParen 0.3..."
                                 #:updated-epoch-seconds
				 (current-seconds)
                                 #:url "http://blog.../51814971"
                                 #:content "Tonight I..."))))
]

Note that while using @scheme[current-seconds] for timestamps does satisfy the interface, it's not really appropriate since these times are supposed to indicated freshness of the data.  If basing your feed off of records, you might consider using @scheme[created-when].

@section{About/Acknowledgements}

LeftParen was written by @link["http://robhunter.org"]{Rob Hunter}, but it builds heavily on (and, in fact, often directly incorporates) the work of @link["http://untyped.com/"]{Untyped} (@link["http://planet.plt-scheme.org/display.ss?package=instaservlet.plt&owner=untyped"]{instaservlet} and @link["http://planet.plt-scheme.org/display.ss?package=dispatch.plt&owner=untyped"]{dispatch}), @link["http://scheme.dk/blog/"]{Jens Axel Soegaard} (@link["http://planet.plt-scheme.org/display.ss?package=web.plt&owner=soegaard"]{web.plt}), and of course, @link["http://www.plt-scheme.org/"]{PLT Scheme}.

@subsection{Contributors}

@itemize[
@item{Bill Hager}
@item{Joshua Herman}
]

