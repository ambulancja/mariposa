#
# Note:
#   $x = e
# is desugared as:
#   at $(now()):
#     x = $(e)

print("--- 1 2 3 4 ---")
l = 1
print(l) # 1
t = now()
print(l) # 2
l = 3
print(l) # 3
at t:
  l = 2
  $l = 4
print(l) # 4

print("--- 1 2 3 2 ---")
l = 1
print(l) # 1
t = now()
print(l) # 2
l = 3
print(l) # 3
at t:
  l = 2
  $l = l
print(l) # 3

