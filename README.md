Table summarizer for large tables in databases
================
Henry Luan

## About this web app

This RShiny app summarizes large tables stored in database servers. It
allows large, high-dimensional datasets to be summarized in R (size of
local RAM \<\<\< size of tables) in a way that R can’t handle with your
computer’s local RAM.

If you want to understand the core functions used to create this web
app, you can go to my other [github
page](https://github.com/casualcomputer/sql.mechanic) for understanding
the package `sql.mechanic` that I created.

To use the RShiny app, just download this folder and run
“table-summarizer.R”.

## Limitations

- You probably want to understand how [**Example
  2**](https://github.com/casualcomputer/sql.mechanic#example-2-automatically-summarize-tables-in-your-databases)
  (part of my package called “sql.mechanic”) affects the CPU and disk
  usage of your database server, to avoid bad surprises on your server’s
  resource usage.

- If you use the setup of [**Example
  2**](https://github.com/casualcomputer/sql.mechanic#example-2-automatically-summarize-tables-in-your-databases)
  (part of my package called “sql.mechanic”) on a cloud database, you
  **MUST** do some testing, to understand how the example affects the
  CPU and disk usage of your cloud resource. Please avoid potentially
  expensive mistakes.

- Currently, I designed the app to work only with Microsoft SQL Server
  and Netezza databases. Feel free to contribute to the codes, if
  interested.
