% Fluidinfo Developer Competition
% Eric Seidel
% Tue, 05 Apr 2011 17:52:17 -05:00

I've finally had time to work on something for the
[Fluidinfo & O'Reilly API Competition][api-comp], which ends this weekend.
The guys at [Fluidinfo][fi] recently imported tons of metadata about
[O'Reilly Books][oreilly] and they want to see how people will use it.
My idea is to integrate the data with my Resumé project, [FluidCV][].
FluidCV has been missing a skills section for a while since I haven't had
time to work on it between school, internships, etc. So, instead of a
traditional (boring?) list of skills, why not list O'Reilly books that
match my skills.

What I did was tag a few books about Python, Perl, Unix, and Git with
`gridaphobe/skill`. Then I told FluidCV to find the books I had
tagged, and retrieve the `title`, `homepage`, and `cover-small` tags
under the `oreilly.com` namespace. This way I can construct a nice
group of book-covers (with on-hover titles and a link to the book's
homepage) that represent my skills. Of course this isn't just for me,
anyone can create their own [FluidCV][] by following the instructions
on the main page.

<strike>One note, I haven't yet added the ability to add these tags in the FluidCV
edit pages; hopefully I'll have time to do that this weekend before the
competition deadline.</strike>
The edit page now has a nice autocompleting
form for adding skills! Take a look at [my FluidCV][mine] for an idea of what
the end result will look like.

Update: Some technical details for those interested.

- FluidCV is written in python and runs on Google App Engine.
- There is __no__ local datastore. Everything except for the HTML
  layout comes from Fluidinfo.
- There's no reason the layout can't be stored in Fluidinfo. This is
  actually how I would implement user-customizable layouts if/when
  people start to use FluidCV.
- I used the following tags from the O'Reilly data:

  - `oreilly.com/title`
  - `oreilly.com/homepage`
  - `oreilly.com/cover-small`

- When you navigate to <http://fluid-cv.appspot.com/gridaphobe>,
  FluidCV executes the following query to find my skills.

  - `has gridaphobe/skill and has oreilly.com/title`[^1]

- Then FluidCV grabs the aforementioned tags and inserts them into the HTML layout.

It's actually really simple, although the skill editing page is a bit more complicated.

I used the [jQuery Token Input library][jqti] to make a nice
autocomplete form for adding skills (i.e. you type "Python" and it gives you a
list of all books that have Python in the title) and a simple [jQuery][jq]
call to populate the list of all books once the page is loaded. The
autocomplete form gives me a list of Object [UUIDs][uuid] corresponding to the
books the user chose, which I can then tag with `username/skill`.


[^1]: I could just use `has gridaphobe/skill` but I could potentially
be tagging objects other than O'Reilly books with `gridaphobe/skill`.


[api-comp]: http://radar.oreilly.com/2011/03/api-competition.html
[fi]: http://fluidinfo.com
[oreilly]: http://oreilly.com/
[FluidCV]: http://fluid-cv.appspot.com
[mine]: http://fluid-cv.appspot.com/gridaphobe
[jqti]: http://loopj.com/jquery-tokeninput/
[jq]: http://jquery.com/
[uuid]: http://en.wikipedia.org/wiki/Uuid
