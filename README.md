# Meatbar Analytics

This is Lane Seppala's submission for the Front Row 'Meatbar Analytics'
project.

I have never had an Epic meat bar before, but after typing "meatbar"
over and over for this project, I have cravings I fear cannot be sated.


## Setup

Depends on `stack`, v1.4.0 or later.

The Makefile has targets to build and run the application. Run
```shell
make run
```
in the project directory to build and start the server on port **8080**.


If `make` is not available, run
```shell
stack build && stack exec meatbar
```
in the project directory to start the server.

## Routes

The following routes have been implemented, as required.

* `GET /person`: list of all person stored in the database

* `GET /consumption`:  list of all meatbar consumption events

* `POST /consumption`: create a meatbar consumption event. The JSON request paylod
  format is as follows:
    ```
    { "meatType": <string>,
      "consumedAt": <ISO 8601 date formated string>,
      "personId": <integer>
    }
    ```

* `GET /consumption/streaks`: list all of the meatbar consumption streaks. A
  streak is defined as at least two consecutive days where the consumption
  count was greater than the day before

* `GET /consumption/month_stats`: for each month, list the day of the month
  with the highest number of meatbar consumption events


## Tests

Test can be run with `make test` or `stack test`. Tests are pure (no IO) and
test the consumption streak and monthly stats logic.


## Design decisions

I'll take a few seconds here to explain some of my design decisions:

* This code certainly could have been collapsed into a small handful of files
  rather than the dozen-odd I have here. I chose to demonstrate my refactoring
  and organization abilities, rather than to optimize for completing the
  problem in as few files as possible. I think this more accurately reflects
  real-world software engineering solutions when dealing with growing projects.

* The same goes for my chosen use of MTL type classes and monad transformers. I
  could have gone for simpler types, but I feel the *reasonable* use of type
  class contraints and monad transformer stacks allows for projects to grow and
  remain well organized.

* I choose to hard-code the port and CSV data filename in the Main.hs file for
  simplicity. Hopefully this isn't an interview trap I fell into :) Will
  demonstrate mad `optparse-applicative` skills if necessary.
