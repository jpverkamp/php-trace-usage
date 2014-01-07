php-trace-usage
===============

Trace function calls in PHP to endpoints (either returned or echo'ed)

Usage:

    trace-usage [ <option> ... ]
     where <option> is one of
      --source <directory> : Set the project root directory
      --function <function-name> : Original function call to trace
      --filter <filter-re> : Only return endpoints matching this regular expression
      --debug, --DEBUG : Set debug mode
      --help, -h : Show this help
