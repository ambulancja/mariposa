# Example due to mega
# Multiple time travelers error
t = now()
at t:
  print(1)
  at $t:
    print(2)
