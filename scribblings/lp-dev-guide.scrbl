#lang scribble/doc
@(require scribble/manual)

@title{LeftParen Developer's Guide}

This is a document for people who are contributing to the LeftParen source code, not for those who are simply using LeftParen to make web apps.

@section{Styleguide}

See @link["http://github.com/vegashacker/leftparen/tree/master/STYLE.txt"]{http://github.com/vegashacker/leftparen/tree/master/STYLE.txt} for guidelines around the code you write for LeftParen.

@section{How to submit a LeftParen patch}

@italic{Disclaimer:} I'm new to git.  If you see that I'm doing things non-optimally please send me a note (@tt{support at leftparen.com}).

You'll want to familiarize yourself with the basics of GitHub, which is where the LeftParen source is hosted.  

@subsection{Create your own sandbox (fork)}

In the GitHub UI, create a fork of the LeftParen project, which is at @link["http://github.com/vegashacker/leftparen/tree/master"]{http://github.com/vegashacker/leftparen/tree/master}.

Get the source on your location machine:

@verbatim{
% git clone git://github.com/vegashacker/leftparen.git
}

This will create a directory called @tt{leftparen} on your local machine.

@subsection{Creating a branch}

Right now I (Rob) soley evaluate all submitted LeftParen patches.  To keep the process running along smoothly (with high-quality patches getting into mainline LeftParen quickly), and to preserve my sanity, I require that each patch (a bug fix, a particular feature, etc) gets its own branch.  If you try to bundle a bunch of bug fixes and/or feature additions together, it's harder for me to evaluate the patch.  Small patches are less-error prone, and get through the system faster.

Unless your only purpose is to submit a single patch to leftparen (and not continue on, say, future patches), you should make a separate (non-master) branch to do your work on.  This way, you can iterate on your patch, committing changes to the branch, and, when done, you can point me to the branch and I can evaluate it separate from other changes you might be working on.  Alternatively, if you did your patch on the master branch, you would essentially have to stop all other LeftParen development because I will only look at single feature/bug patches.

As an example of the branch and patch process, I'm going to fix a typo in this file.
