# Top Movies
How to Run:
  1. checkout or download the movie folder onto your ITL machine
  2. cd into the movie folder and run: /homes/YOUR_ITL_USERNAME/.local/bin/stack build
  3. Then run: /homes/YOUR_ITL_USERNAME/.local/bin/stack exec topmovies-exe setupDatabase
  4. Once the database is setup, run: /homes/YOUR_ITL_USERNAME/.local/bin/stack exec topmovies-exe showMovies
  5. To view the database in a readable format go to: http://inloop.github.io/sqlite-viewer/ and open DB.db
  
Generating Haddock Docs:

  /homes/YOUR_ITL_USERNAME/.local/bin/stack exec -- haddock --html src/*.hs app/Main.hs --hyperlinked-source --odir=dist/docs
