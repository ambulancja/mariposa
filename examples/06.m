
t = now()
at t:
  t = ($t, $t)
  print(t)
print(t)

# Correctly returns:
#
#   (<instant 2>, <instant 2>)
#   <instant 2>
#
# Note that the assignment t = ($t,$t)
# is done at the point where now() is called,
# but *before* the assignment t = now()
