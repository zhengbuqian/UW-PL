fun calculate_interest(n) = if n < 0.0
                            then raise Domain
                            else n * 1.04
val balance = calculate_interest ~180.0
              handle Domain => ~180.0