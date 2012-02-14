% FluidCV
% Eric Seidel
% Fri, 11 Feb 2011 00:00:00 GMT

I'm starting a new project called [FluidCV][fcv]. It will be a new way to
create a resumé/cv, using [Fluidinfo][fdb] as the backend storage. Fluidinfo is an
open, social database, where objects are readable and writeable by everyone.
Objects are essentially anonymous, except for a unique *about* tag. Users can
tag objects with any tag they want, and have full control over who can read or
write to their tags. Fluidinfo also provides a simple query language for finding
objects. Using this language, I could for example search for a tech blog post
that I really like with

[fcv]: http://fluid-cv.appspot.com
[fdb]: http://fluidinfo.com

> gridaphobe/rating > 7 and gridaphobe/post/category="technology"

This might return an object with the about tag <http://lifehac.kr/g1KtPo>,
which links to a nice post about a new Gmail client for Mac OS X.

Now, if I'm going to tag blog posts that I like, why not tag places I went to
school or at which I worked? I used to be a Genius at [Apple][], so I tagged
the object with about tag <http://www.apple.com> with the tags

[Apple]: http://www.apple.com

* gridaphobe/employer
* gridaphobe/company = "Apple Inc."
* gridaphobe/title = "Genius"
* gridaphobe/start-date = "2008-02-15"
* gridaphobe/end-date = "2009-07-15"
* gridaphobe/functions = \['Diagnosed and resolved customer issues with full range of Apple products.', 'Hired as Specialist, promoted to Genius in Sep. 2008.'\]

I'm currently a student at [The City College of New York][ccny], so I also
tagged the object with about tag <http://ccny.cuny.edu> with the tags

[ccny]: http://ccny.cuny.edu

* gridaphobe/attended
* gridaphobe/school-name = "The City College of New York"
* gridaphobe/school-location = "New York, NY"
* gridaphobe/major = "Computer Science"
* gridaphobe/gpa = 3.95
* gridaphobe/start-date = "2009-08-28"
* gridaphobe/end-date = "2012-05-25"

Now I have some nice metadata about where I worked and went to school, but
I'm missing the most important part of a resume, my contact info! The solution
is simple; every Fluidinfo user has a special object representing them. It has
the about tag 'Object for the user named $user.' So I tagged my user object
with

* gridaphobe/given-name = "Eric"
* gridaphobe/family-name = "Seidel"
* gridaphobe/cell-phone = "+1 225 276 2830"
* gridaphobe/email = "eric@eseidel.org"
* gridaphobe/summary = "Seeking opportunities to further skills and
  experiences in research that leverage my expertise in software
  design and implementation, particularly in a multidisciplinary and
  collaborative environment."

With all the metadata in place (I've had a few internships since I worked at
Apple, but this is a simple example) I wrote a simple webapp using [Flask][],
[Google App Engine][GAE], and the fantastic [Fluid Object Mapper][FOM], which
grabs the metadata from Fluidinfo and places it in a nice template that I found
online. It also marks up the resulting resumé using the [hResume][hr] format,
a microformat for writing resumé metadata.

[Flask]: http://flask.pocoo.org
[GAE]: http://code.google.com/appengine/
[FOM]: https://launchpad.net/fom
[hr]: http://microformats.org/wiki/hresume

Right now [my webapp][fcv] only shows my info, and doesn't allow me to add new
info via a nice web-interface, but that will come soon. I also plan to make it
work for anyone with a Fluidinfo account and the correct tags. Feel free to
check out the source at [GitHub][gh].

[gh]: https://github.com/gridaphobe/fluid-cv
