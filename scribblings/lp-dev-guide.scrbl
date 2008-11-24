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

In the GitHub UI, create a fork of the LeftParen project.  LeftParen is located at @link["http://github.com/vegashacker/leftparen/tree/master"]{http://github.com/vegashacker/leftparen/tree/master}.

Grab the LeftParen source:

@verbatim{
% git clone git://github.com/vegashacker/leftparen.git
}

This will create a directory called @tt{leftparen} on your local machine.

@subsection{Creating a branch}

Right now I (Rob) evaluate all proposed LeftParen patches.  To keep the process running along smoothly (with high-quality patches getting into mainline LeftParen quickly), and to preserve my sanity, I require that each patch (a bug fix, a particular feature, etc) gets its own branch.  If you try to bundle a bunch of bug fixes and/or feature additions together, it's hard for me to evaluate an individual patch.  Small, isloated patches are less-error prone, and get through the system faster.

Unless your only purpose is to submit a single patch to LeftParen (and not to continue working on future LeftParen patches), you should make a separate (non-master) branch to do your work on.  This way, you can iterate on your patch, committing changes to the branch.  When you're done, you can point me to the branch and I can evaluate it separately from any other changes you might be working on.  Alternatively, if you did your patch on the master branch, you would essentially have to stop all other LeftParen development because I will only look at single feature/bug patches.

As an example of the branch and patch process, I'm going to fix a typo in this file.  First, I create a branch to do some development work on.  I'll call my branch @tt{dev}, in anticipation of it serving as a general purpose development branch.  You might also want to consider creating a branch with a name specific to the patch you are creating--e.g., @tt{fixing-that-typo}.  Once the patch was approved and merged into mainline LeftParen, you would probably want to delete the @tt{fixing-that-typo} branch.

@verbatim{
% git branch dev (creates a branch named "dev")
% git checkout dev (switches to the "dev" branch)
}

@subsection{Writing and applying your patch}

I now make the changes I want, using @tt{git add} and @tt{git commit} as needed.  And of course, I use @tt{git diff} often to make sure my patch is what I think it is.  In this case, I only did one small typo fix, and so I just entered

@verbatim{
% git diff (to make sure it looked right)
% git add scribblings/lp-dev-guide.scrbl
% git commit -m "fixed LeftParen improper capitalization"
% git push origin dev
}

The last command actually sent @link["http://github.com/vegashacker/leftparen/commit/92609b3dc3779d245dae29ad32c45781119d5c83"]{my patch} to GitHub.  At this point in the process, I would send an email to @tt{support at leftparen dot com}, saying something like

@verbatim{
Hey Rob,


Check out my patch for LeftParen which fixes one of the worst typos I've ever seen.  Because you are totally insane, I have put this patch on its own branch (http://github.com/vegashacker/leftparen/tree/dev).  Enjoy!


Thanks,
Rob
}

I will then evaluate the patch, and if it all looks good, I'll merge it into the LeftParen master branch.

@subsection{Keeping in sync with master}

As you are developing on your branch, the code may be moving forward on mainline (@tt{vegashacker/master}).  PLEASE HELP ME UPDATE THIS SECTION IF YOU GET THIS TO WORK.  Something like this should do the trick:

@verbatim{
% git checkout YOUR_BRANCH (make sure you are on your branch)
% git stash (if you have any changes that aren't checked in)
% git pull . git://github.com/vegashacker/leftparen.git (not sure about this line because so far I've only tested it locally, on the same repo; this line should trigger the merge).
% git stash apply (to re-apply the changes you stashed above)
}

Again, let me know if there's a better/correct way of doing this.  You'll want to do this often (or, rather, as often as mainline moves forward).
