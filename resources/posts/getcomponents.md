% What is GetComponents?
% Eric Seidel
% Sun, 27 Feb 2011 00:00:00 -05:00

I've never written a blog post about [GetComponents][gc], so lets fix
that. I started working on Getcomponents almost a year ago with the
[Center for Computation & Technology][cct] at Louisiana State
University. Some of the main developers for the
[Cactus Computational Toolkit][cactus] work at CCT, and needed a fresh
way to distribute their framework. Cactus is a distributed framework,
in the sense that it is composed of many smaller pieces of code that
could theoretically function on their own. Add in the fact that these
modules, or "Thorns", are developed by many different people, and you
can see why the standard tarball distribution doesn't make sense for
Cactus.

[gc]: http://eseidel.org/projects/getcomponents
[cct]: http://cct.lsu.edu
[cactus]: http://cactuscode.org

Instead, they had a clever Perl script called GetCactus that could read a file
containing the URLs of all the needed thorns and checkout the source code from
a variety of cvs repositories. A brilliant solution, but there were some
problems.

1. The script was written in 1999, before svn was even released.
   The initial implementation only supported cvs, which is pretty
   terrible by modern standards. Svn support was easy enough to add since
   svn's design and syntax are nearly identical to cvs. When git came along,
   however, it was much more difficult to support. Git (and DVCS's in general)
   has a completely different design and mentality compared to cvs and svn,
   and while some git support was added to GetCactus, it wasn't as elegant as
   they would have liked.

2. There were some portability issues in the "Thornlists." The files contained
   usernames and passwords, which had to be changed each time a user
   downloaded the new file. This caused confusion among users, especially the
   newer ones. Who wants to manually edit a file just to start downloading the
   code?

So they brought me in to rewrite GetCactus. We wanted an extensible,
framework-agnostic tool that could retrieve components from a variety of
VCS's as well as regular http downloads. So we designed the
[Component Retrieval Language][crl].

[crl]: http://github.com/gridaphobe/crl/wiki/Component-Retrieval-Language

## The Component Retrieval Language

The Component Retrieval Language is a Domain Specific Language that we wrote
to solve this problem. It uses about 10 simple directives to identify the URL
of a repository, the version control system being used, the target location on
the local machine, etc. Directives are prefixed by a `!` and variables can be
declared to simplify writing the component list. CRL also allows variable
expansion based on the items to be checked out so you can group a number of
repositories that have similar URL structure as in the following example.

    !TARGET   = $ARR
    !TYPE     = svn
    !AUTH_URL = https://svn.cactuscode.org/arrangements/$1/$2/trunk
    !URL      = http://svn.cactuscode.org/arrangements/$1/$2/trunk
    !CHECKOUT =
    CactusArchive/ADM
    CactusBase/Boundary
    CactusBase/CartGrid3D
    CactusBase/CoordBase

This is an excerpt from the component list for the [Eintstein Toolkit][et],
which uses GetComponents as its means of distribution. It exhibits a couple
nice features of CRL, variable substitution in the URL directive, and variable
definitions (recursive definitions in this case) in the TARGET directive.
Earlier in the file the two lines

    !DEFINE ROOT = Cactus
    !DEFINE ARR  = $ROOT/arrangements

define the target `$ARR` in terms of the `$ROOT` variable, which we just set to
"Cactus." This allows for quite a bit of flexibility in writing the component
lists since the variables are used globally.

If you want to know more about the Component Retrieval Language, I'd suggest
looking at the [wiki][crl], or the [paper][] we published on it.

[paper]: /download/crl-tg10.pdf
[et]: http://www.einsteintoolkit.org

## GetComponents

GetComponents is my implementation of the Component Retrieval Language. It is
written in Perl (my first experience with the language) and currently supports
cvs, svn, git, mercurial, darcs, and http downloads.[^1] GetComponents was
actually my first real programming project outside of homework for school, so
I'm quite happy with the result. It took a while to get to a "complete"
state,[^2]
but it has been a great experience! I can't stress
enough how much of a difference it makes to me as a programmer to know that
people are actually using (and enjoying!) my software, which unfortunately is not
something that most college students get to experience.

[^1]: Note: CRL is essentially system agnostic. The only distinctions it makes are between centralized and decentralized systems.
[^2]: I started working on GetComponents last February and it replaced the GetCactus script a couple months later, but I only felt comfortable tagging a 1.0 release a couple months ago.

Back to GetComponents' functionality... It has some "standard" features like
a verbose mode that prints each command being executed and the output, a debug mode
that shows what would have been done in a real run, and a status and diff
mode. These are a nice addition in my opinion, they inform the user of
modifications and incoming updates, or a diff, for all the repositories that GetComponents
is tracking. This is really helpful because if you are working with many
repositories, it's easy to forget to commit one of them, which could break the
code for everyone except yourself. GetComponents also has a parallel mode for
checking out up to 4 components at a time; in my experience it results in
a 50-70% speed boost over sequential checkouts!

I think that's enough for now, but as a final note I'd like to point out that
GetComponents and CRL can be used for more than just managing code. If you
have any large group of resources (documents, pictures, code, music, etc.)
stored online somewhere, whether in a versioned repository or just on a server
that you can access via http/ftp, you can use GetComponents to simplify retrieving
your stuff. One example is publications. Many scientific papers are written in LaTeX
and stored in separate repositories. If you're a prolific author, it might be
tedious getting a copy of all your papers on your nice new computer.
GetComponents makes that easy! All you have to do is maintain a single
component list with all your papers, and suddenly you only have to download
*one* file and issue *one* terminal command.
