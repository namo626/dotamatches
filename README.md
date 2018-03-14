# dotamatches

A command line Dota 2 match reporter. Scrapes live and upcoming match information from https://www.gosugamers.net/dota2/gosubet .

## Usage:
Build the program:

`stack build`

Run:

`stack exec dotamatches -- [OPTIONS]`

## Examples:

For help, use:

`stack exec dotamatches -- --help`

View only live games and current scores:

`stack exec dotamatches -- --live --spoil`
