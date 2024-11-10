# Functional and TDD

Following the [Global Day of Code Retreat](https://www.coderetreat.org/) we ran in Nice. I want to try this using [TDD](https://en.wikipedia.org/wiki/Test-driven_development) and the [four rule of simple design](https://www.martinfowler.com/bliki/BeckDesignRules.html).

## TDD

- Write a test
- Check it fails
- Write code to pass the test
- Refactor code if you think it's required
- Restart...

## Extreem TDD

- Write a test
- Check it fails
- Write code that only passes the test, dirty style `return true`
- Write the next test
- Only fix the dirty function when you _must_
- Repeat

## Four rules of simple design

-Passes the tests
-Reveals intention
-No duplication
-Fewest elements
