# TODO

- Tables: probably overdoing it with `reactive` constructs. Maybe just deal with reactive for variables and make simple dataframe inside renderTable({})
- Architecture of functions. To think about:
  - For now, most of functions start again from (FPF, TPF). This is just easy to remember. Alternative could be to just call sequentially
  - For now, modules returns the minimal set of reactive (reactVal). This implies repeating some (cheap) computations... Alternative would be to make and pass secondary reactives through the modules and app...
  