This package is an attempt to bring programmatic query construction as in SQLAlchemy to R.  Tables are represented as S4 objects, with the `$` operator overloaded to allow query construction:

    > library(rsql)
    > tab = rsql_table('test',c('x','y','z'))
    > to_sql(tab$select())
    [1] "SELECT test.x AS x,\n\ttest.y AS y,\n\ttest.z AS z\nFROM test"
    > writeLines(to_sql(tab$select()))
    SELECT test.x AS x,
      test.y AS y,
      test.z AS z
    FROM test
    > writeLines(to_sql(tab$select(.(a=x,b=y>5))$where(from(tab,.(z <5)))))
    SELECT test.x AS a,
      test.y > 5 AS b
    FROM test
    WHERE test.z < 5

An ever-expanding list of SQL expressions are supported.  For me information see the package documentation, especially for the `rsql` function.
