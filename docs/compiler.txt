Test tasks for Compilers
     
1. Hello, world/Text
   
Input to lex

/*
  Hello world
 */
print("Hello, World!\n")
     
Output from lex, input to parse
       
    4      1 Keyword_print
    4      6 LeftParen
    4      7 String         "Hello, World!\n"
    4     24 RightParen
    4     25 Semicolon
    5      1 End_of_input
    
Output from parse
       
Sequence
;
Sequence
;
Prts
String        "Hello, World!\n"
;

Output from gen, input to vm
       
Datasize: 0 Strings: 1
"Hello, World!\n"
   0 push  0
   5 prts
   6 halt
   
Output:

Hello, World!
   
2. Phoenix Number
   
Input to lex
      
/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, "\n");       
                      
.
.
.
        
Output from gen, input to vm
       
Datasize: 1 Strings: 1
"\n"
   0 push  142857
   5 store [0]
  10 fetch [0]
  15 prti
  16 push  0
  21 prts
  22 halt
  
Output:

142857
  
3. All Symbols
  
.
.
.

4. Test Case 4
   
Input to lex
  
/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");    
             
.
.
.
Output from gen, input to vm
       
Datasize: 0 Strings: 2
"\nHello World\nGood Bye\nok\n"
"Print a slash n - \\n.\n"
   0 push  42
   5 prti
   6 push  0
  11 prts
  12 push  1
  17 prts
  18 halt  
  
Output:

42
Hello World
Good Bye
ok
Print a slash n - \n.
      
5. Count
   
Input to lex
      
count = 1;
while
(count < 10) {
print("count
count,
"\n");
count + 1;     
}

.
.
.
Output from gen, input to vm
       
Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt  
  
Output:

count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9      
      
6. 100_doors
   
Input to lex:
      
/* 100 Doors */
i = 1;
while (i * i <= 100) {
    print("door ", i * i, " is open\n");
    i = i + 1;
}    
    
.
.
.
Output from gen, input to vm
       
lex ..\100doors.t | parse | gen
Datasize: 1 Strings: 2
"door "
" is open\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 fetch [0]
   20 mul
   21 push  100
   26 le
   27 jz     (49) 77
   32 push  0
   37 prts
   38 fetch [0]
   43 fetch [0]
   48 mul
   49 prti
   50 push  1
   55 prts
   56 fetch [0]
   61 push  1
   66 add
   67 store [0]
   72 jmp    (-63) 10
   77 halt   
   
Output

door 1 is open
door 4 is open
door 9 is open
door 16 is open
door 25 is open
door 36 is open
door 49 is open
door 64 is open
door 81 is open
door 100 is open     
     
7. Negative Tests
   
Input to lex
      
a = (-1 * ((-1 * (5 * 15)) / 10));
print(a, "\n");
b = -a;
print(b, "\n");
print(-b, "\n");
print(-(1), "\n");      
            
.
.
.

Output from gen, input to vm
       
Datasize: 2 Strings: 1
"\n"
   0 push  1
   5 neg
   6 push  1
  11 neg
  12 push  5
  17 push  15
  22 mul
  23 mul
  24 push  10
  29 div
  30 mul
  31 store [0]
  36 fetch [0]
  41 prti
  42 push  0
  47 prts
  48 fetch [0]
  53 neg
  54 store [1]
  59 fetch [1]
  64 prti
  65 push  0
  70 prts
  71 fetch [1]
  76 neg
  77 prti
  78 push  0
  83 prts
  84 push  1
  89 neg
  90 prti
  91 push  0
  96 prts
  97 halt  
  
Output

7
-7
7
-1

8. Deep
   
Input to lex
      
print(---------------------------------+++5, "\n");
print(((((((((3 + 2) * ((((((2))))))))))))), "\n");

if (1) { if (1) { if (1) { if (1) { if (1) { print(15, "\n"); } } } } }
   
.
.
.

Output from gen, input to vm
       
Datasize: 0 Strings: 1
"\n"
   0 push  5
   5 neg
   6 neg
   7 neg
   8 neg
   9 neg
  10 neg
  11 neg
  12 neg
  13 neg
  14 neg
  15 neg
  16 neg
  17 neg
  18 neg
  19 neg
  20 neg
  21 neg
  22 neg
  23 neg
  24 neg
  25 neg
  26 neg
  27 neg
  28 neg
  29 neg
  30 neg
  31 neg
  32 neg
  33 neg
  34 neg
  35 neg
  36 neg
  37 neg
  38 prti
  39 push  0
  44 prts
  45 push  3
  50 push  2
  55 add
  56 push  2
  61 mul
  62 prti
  63 push  0
  68 prts
  69 push  1
  74 jz     (56) 131
  79 push  1
  84 jz     (46) 131
  89 push  1
  94 jz     (36) 131
  99 push  1
 104 jz     (26) 131
 109 push  1
 114 jz     (16) 131
 119 push  15
 124 prti
 125 push  0
 130 prts
 131 halt 
 
Output

-5
10
15

9. Greatest Common Divisor
   
Input to lex
      
/* Compute the gcd of 1071, 1029:  21 */

a = 1071;
b = 1029;

while (b != 0) {
    new_a = b;
    b     = a % b;
    a     = new_a;
}
print(a);  
            
.
.
.

Output from gen, input to vm
       
Datasize: 3 Strings: 0
    0 push  1071
    5 store [0]
   10 push  1029
   15 store [1]
   20 fetch [1]
   25 push  0
   30 ne
   31 jz     (45) 77
   36 fetch [1]
   41 store [2]
   46 fetch [0]
   51 fetch [1]
   56 mod
   57 store [1]
   62 fetch [2]
   67 store [0]
   72 jmp    (-53) 20
   77 fetch [0]
   82 prti
   83 halt
   
Output

21


10. Factorial
    
Input to lex
      
/* 12 factorial is 479001600 */

n = 12;
result = 1;
i = 1;
while (i <= n) {
    result = result * i;
    i = i + 1;
}
print(result);

.
.
.

Output from gen, input to vm
       
Datasize: 3 Strings: 0
    0 push  12
    5 store [0]
   10 push  1
   15 store [1]
   20 push  1
   25 store [2]
   30 fetch [2]
   35 fetch [0]
   40 le
   41 jz     (41) 83
   46 fetch [1]
   51 fetch [2]
   56 mul
   57 store [1]
   62 fetch [2]
   67 push  1
   72 add
   73 store [2]
   78 jmp    (-49) 30
   83 fetch [1]
   88 prti
   89 halt
   
Output:

479001600


11. Fibonacci Sequence
    
Input to lex
      
/* fibonacci of 44 is 701408733 */

n = 44;
i = 1;
a = 0;
b = 1;
while (i < n) {
    w = a + b;
    a = b;
    b = w;
    i = i + 1;
}
print(w, "\n");
         
.
.
.

Output from gen, input to vm
       
Datasize: 5 Strings: 1
"\n"
    0 push  44
    5 store [0]
   10 push  1
   15 store [1]
   20 push  0
   25 store [2]
   30 push  1
   35 store [3]
   40 fetch [1]
   45 fetch [0]
   50 lt
   51 jz     (61) 113
   56 fetch [2]
   61 fetch [3]
   66 add
   67 store [4]
   72 fetch [3]
   77 store [2]
   82 fetch [4]
   87 store [3]
   92 fetch [1]
   97 push  1
  102 add
  103 store [1]
  108 jmp    (-69) 40
  113 fetch [4]
  118 prti
  119 push  0
  124 prts
  125 halt  
  
Output

701409733

12. FizzBuzz
    
Input to lex
      
/* FizzBuzz */
i = 1;
while (i <= 100) {
    if (!(i % 15))
        print("FizzBuzz");
    else if (!(i % 3))
        print("Fizz");
    else if (!(i % 5))
        print("Buzz");
    else
        print(i);

    print("\n");
    i = i + 1;
}    
    
.
.
.

Output from gen, input to vm
       
Datasize: 1 Strings: 4
"FizzBuzz"
"Fizz"
"Buzz"
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  100
   20 le
   21 jz     (121) 143
   26 fetch [0]
   31 push  15
   36 mod
   37 not
   38 jz     (15) 54
   43 push  0
   48 prts
   49 jmp    (66) 116
   54 fetch [0]
   59 push  3
   64 mod
   65 not
   66 jz     (15) 82
   71 push  1
   76 prts
   77 jmp    (38) 116
   82 fetch [0]
   87 push  5
   92 mod
   93 not
   94 jz     (15) 110
   99 push  2
  104 prts
  105 jmp    (10) 116
  110 fetch [0]
  115 prti
  116 push  3
  121 prts
  122 fetch [0]
  127 push  1
  132 add
  133 store [0]
  138 jmp    (-129) 10
  143 halt
         
Output

1
2
Buzz
4
Fizz
Buzz
7
8
Buzz
Fizz
11
Buzz
13
14
FizzBuzz
16
17
Buzz
19
Fizz
Buzz
22
23
Buzz
Fizz
26
Buzz
28
29
FizzBuzz
31
32
Buzz
34
Fizz
Buzz
37
38
Buzz
Fizz
41
Buzz
43
44
FizzBuzz
46
47
Buzz
49
Fizz
Buzz
52
53
Buzz
Fizz
56
Buzz
58
59
FizzBuzz
61
62
Buzz
64
Fizz
Buzz
67
68
Buzz
Fizz
71
Buzz
73
74
FizzBuzz
76
77
Buzz
79
Fizz
Buzz
82
83
Buzz
Fizz
86
Buzz
88
89
FizzBuzz
91
92
Buzz
94
Fizz
Buzz
97
98
Buzz
Fizz

13. Bottles of Beer
    
Input to lex
      
/* 99 bottles */
bottles = 99;
while (bottles > 0) {
    print(bottles, " bottles of beer on the wall\n");
    print(bottles, " bottles of beer\n");
    print("Take one down, pass it around\n");
    bottles = bottles - 1;
    print(bottles, " bottles of beer on the wall\n\n");
}    
    
.
.
.

Output from gen, input to vm
       
Datasize: 1 Strings: 4
" bottles of beer on the wall\n"
" bottles of beer\n"
"Take one down, pass it around\n"
" bottles of beer on the wall\n\n"
    0 push  99
    5 store [0]
   10 fetch [0]
   15 push  0
   20 gt
   21 jz     (67) 89
   26 fetch [0]
   31 prti
   32 push  0
   37 prts
   38 fetch [0]
   43 prti
   44 push  1
   49 prts
   50 push  2
   55 prts
   56 fetch [0]
   61 push  1
   66 sub
   67 store [0]
   72 fetch [0]
   77 prti
   78 push  3
   83 prts
   84 jmp    (-75) 10
   89 halt   
   
Output
     
100 bottles of beer on the wall,
100 bottles of beer,
Take one down, pass it around,
99 bottles of beer on the wall.

99 bottles of beer on the wall,
99 bottles of beer,
Take one down, pass it around,
98 bottles of beer on the wall.

98 bottles of beer on the wall,
98 bottles of beer,
Take one down, pass it around,
97 bottles of beer on the wall.

97 bottles of beer on the wall,
97 bottles of beer,
Take one down, pass it around,
96 bottles of beer on the wall.

96 bottles of beer on the wall,
96 bottles of beer,
Take one down, pass it around,
95 bottles of beer on the wall.

95 bottles of beer on the wall,
95 bottles of beer,
Take one down, pass it around,
94 bottles of beer on the wall.

94 bottles of beer on the wall,
94 bottles of beer,
Take one down, pass it around,
93 bottles of beer on the wall.

93 bottles of beer on the wall,
93 bottles of beer,
Take one down, pass it around,
92 bottles of beer on the wall.

92 bottles of beer on the wall,
92 bottles of beer,
Take one down, pass it around,
91 bottles of beer on the wall.

91 bottles of beer on the wall,
91 bottles of beer,
Take one down, pass it around,
90 bottles of beer on the wall.

90 bottles of beer on the wall,
90 bottles of beer,
Take one down, pass it around,
89 bottles of beer on the wall.

89 bottles of beer on the wall,
89 bottles of beer,
Take one down, pass it around,
88 bottles of beer on the wall.

88 bottles of beer on the wall,
88 bottles of beer,
Take one down, pass it around,
87 bottles of beer on the wall.

87 bottles of beer on the wall,
87 bottles of beer,
Take one down, pass it around,
86 bottles of beer on the wall.

86 bottles of beer on the wall,
86 bottles of beer,
Take one down, pass it around,
85 bottles of beer on the wall.

85 bottles of beer on the wall,
85 bottles of beer,
Take one down, pass it around,
84 bottles of beer on the wall.

84 bottles of beer on the wall,
84 bottles of beer,
Take one down, pass it around,
83 bottles of beer on the wall.

83 bottles of beer on the wall,
83 bottles of beer,
Take one down, pass it around,
82 bottles of beer on the wall.

82 bottles of beer on the wall,
82 bottles of beer,
Take one down, pass it around,
81 bottles of beer on the wall.

81 bottles of beer on the wall,
81 bottles of beer,
Take one down, pass it around,
80 bottles of beer on the wall.

80 bottles of beer on the wall,
80 bottles of beer,
Take one down, pass it around,
79 bottles of beer on the wall.

79 bottles of beer on the wall,
79 bottles of beer,
Take one down, pass it around,
78 bottles of beer on the wall.

78 bottles of beer on the wall,
78 bottles of beer,
Take one down, pass it around,
77 bottles of beer on the wall.

77 bottles of beer on the wall,
77 bottles of beer,
Take one down, pass it around,
76 bottles of beer on the wall.

76 bottles of beer on the wall,
76 bottles of beer,
Take one down, pass it around,
75 bottles of beer on the wall.

75 bottles of beer on the wall,
75 bottles of beer,
Take one down, pass it around,
74 bottles of beer on the wall.

74 bottles of beer on the wall,
74 bottles of beer,
Take one down, pass it around,
73 bottles of beer on the wall.

73 bottles of beer on the wall,
73 bottles of beer,
Take one down, pass it around,
72 bottles of beer on the wall.

72 bottles of beer on the wall,
72 bottles of beer,
Take one down, pass it around,
71 bottles of beer on the wall.

71 bottles of beer on the wall,
71 bottles of beer,
Take one down, pass it around,
70 bottles of beer on the wall.

70 bottles of beer on the wall,
70 bottles of beer,
Take one down, pass it around,
69 bottles of beer on the wall.

69 bottles of beer on the wall,
69 bottles of beer,
Take one down, pass it around,
68 bottles of beer on the wall.

68 bottles of beer on the wall,
68 bottles of beer,
Take one down, pass it around,
67 bottles of beer on the wall.

67 bottles of beer on the wall,
67 bottles of beer,
Take one down, pass it around,
66 bottles of beer on the wall.

66 bottles of beer on the wall,
66 bottles of beer,
Take one down, pass it around,
65 bottles of beer on the wall.

65 bottles of beer on the wall,
65 bottles of beer,
Take one down, pass it around,
64 bottles of beer on the wall.

64 bottles of beer on the wall,
64 bottles of beer,
Take one down, pass it around,
63 bottles of beer on the wall.

63 bottles of beer on the wall,
63 bottles of beer,
Take one down, pass it around,
62 bottles of beer on the wall.

62 bottles of beer on the wall,
62 bottles of beer,
Take one down, pass it around,
61 bottles of beer on the wall.

61 bottles of beer on the wall,
61 bottles of beer,
Take one down, pass it around,
60 bottles of beer on the wall.

60 bottles of beer on the wall,
60 bottles of beer,
Take one down, pass it around,
59 bottles of beer on the wall.

59 bottles of beer on the wall,
59 bottles of beer,
Take one down, pass it around,
58 bottles of beer on the wall.

58 bottles of beer on the wall,
58 bottles of beer,
Take one down, pass it around,
57 bottles of beer on the wall.

57 bottles of beer on the wall,
57 bottles of beer,
Take one down, pass it around,
56 bottles of beer on the wall.

56 bottles of beer on the wall,
56 bottles of beer,
Take one down, pass it around,
55 bottles of beer on the wall.

55 bottles of beer on the wall,
55 bottles of beer,
Take one down, pass it around,
54 bottles of beer on the wall.

54 bottles of beer on the wall,
54 bottles of beer,
Take one down, pass it around,
53 bottles of beer on the wall.

53 bottles of beer on the wall,
53 bottles of beer,
Take one down, pass it around,
52 bottles of beer on the wall.

52 bottles of beer on the wall,
52 bottles of beer,
Take one down, pass it around,
51 bottles of beer on the wall.

51 bottles of beer on the wall,
51 bottles of beer,
Take one down, pass it around,
50 bottles of beer on the wall.

50 bottles of beer on the wall,
50 bottles of beer,
Take one down, pass it around,
49 bottles of beer on the wall.

49 bottles of beer on the wall,
49 bottles of beer,
Take one down, pass it around,
48 bottles of beer on the wall.

48 bottles of beer on the wall,
48 bottles of beer,
Take one down, pass it around,
47 bottles of beer on the wall.

47 bottles of beer on the wall,
47 bottles of beer,
Take one down, pass it around,
46 bottles of beer on the wall.

46 bottles of beer on the wall,
46 bottles of beer,
Take one down, pass it around,
45 bottles of beer on the wall.

45 bottles of beer on the wall,
45 bottles of beer,
Take one down, pass it around,
44 bottles of beer on the wall.

44 bottles of beer on the wall,
44 bottles of beer,
Take one down, pass it around,
43 bottles of beer on the wall.

43 bottles of beer on the wall,
43 bottles of beer,
Take one down, pass it around,
42 bottles of beer on the wall.

42 bottles of beer on the wall,
42 bottles of beer,
Take one down, pass it around,
41 bottles of beer on the wall.

41 bottles of beer on the wall,
41 bottles of beer,
Take one down, pass it around,
40 bottles of beer on the wall.

40 bottles of beer on the wall,
40 bottles of beer,
Take one down, pass it around,
39 bottles of beer on the wall.

39 bottles of beer on the wall,
39 bottles of beer,
Take one down, pass it around,
38 bottles of beer on the wall.

38 bottles of beer on the wall,
38 bottles of beer,
Take one down, pass it around,
37 bottles of beer on the wall.

37 bottles of beer on the wall,
37 bottles of beer,
Take one down, pass it around,
36 bottles of beer on the wall.

36 bottles of beer on the wall,
36 bottles of beer,
Take one down, pass it around,
35 bottles of beer on the wall.

35 bottles of beer on the wall,
35 bottles of beer,
Take one down, pass it around,
34 bottles of beer on the wall.

34 bottles of beer on the wall,
34 bottles of beer,
Take one down, pass it around,
33 bottles of beer on the wall.

33 bottles of beer on the wall,
33 bottles of beer,
Take one down, pass it around,
32 bottles of beer on the wall.

32 bottles of beer on the wall,
32 bottles of beer,
Take one down, pass it around,
31 bottles of beer on the wall.

31 bottles of beer on the wall,
31 bottles of beer,
Take one down, pass it around,
30 bottles of beer on the wall.

30 bottles of beer on the wall,
30 bottles of beer,
Take one down, pass it around,
29 bottles of beer on the wall.

29 bottles of beer on the wall,
29 bottles of beer,
Take one down, pass it around,
28 bottles of beer on the wall.

28 bottles of beer on the wall,
28 bottles of beer,
Take one down, pass it around,
27 bottles of beer on the wall.

27 bottles of beer on the wall,
27 bottles of beer,
Take one down, pass it around,
26 bottles of beer on the wall.

26 bottles of beer on the wall,
26 bottles of beer,
Take one down, pass it around,
25 bottles of beer on the wall.

25 bottles of beer on the wall,
25 bottles of beer,
Take one down, pass it around,
24 bottles of beer on the wall.

24 bottles of beer on the wall,
24 bottles of beer,
Take one down, pass it around,
23 bottles of beer on the wall.

23 bottles of beer on the wall,
23 bottles of beer,
Take one down, pass it around,
22 bottles of beer on the wall.

22 bottles of beer on the wall,
22 bottles of beer,
Take one down, pass it around,
21 bottles of beer on the wall.

21 bottles of beer on the wall,
21 bottles of beer,
Take one down, pass it around,
20 bottles of beer on the wall.

20 bottles of beer on the wall,
20 bottles of beer,
Take one down, pass it around,
19 bottles of beer on the wall.

19 bottles of beer on the wall,
19 bottles of beer,
Take one down, pass it around,
18 bottles of beer on the wall.

18 bottles of beer on the wall,
18 bottles of beer,
Take one down, pass it around,
17 bottles of beer on the wall.

17 bottles of beer on the wall,
17 bottles of beer,
Take one down, pass it around,
16 bottles of beer on the wall.

16 bottles of beer on the wall,
16 bottles of beer,
Take one down, pass it around,
15 bottles of beer on the wall.

15 bottles of beer on the wall,
15 bottles of beer,
Take one down, pass it around,
14 bottles of beer on the wall.

14 bottles of beer on the wall,
14 bottles of beer,
Take one down, pass it around,
13 bottles of beer on the wall.

13 bottles of beer on the wall,
13 bottles of beer,
Take one down, pass it around,
12 bottles of beer on the wall.

12 bottles of beer on the wall,
12 bottles of beer,
Take one down, pass it around,
11 bottles of beer on the wall.

11 bottles of beer on the wall,
11 bottles of beer,
Take one down, pass it around,
10 bottles of beer on the wall.

10 bottles of beer on the wall,
10 bottles of beer,
Take one down, pass it around,
9 bottles of beer on the wall.

9 bottles of beer on the wall,
9 bottles of beer,
Take one down, pass it around,
8 bottles of beer on the wall.

8 bottles of beer on the wall,
8 bottles of beer,
Take one down, pass it around,
7 bottles of beer on the wall.

7 bottles of beer on the wall,
7 bottles of beer,
Take one down, pass it around,
6 bottles of beer on the wall.

6 bottles of beer on the wall,
6 bottles of beer,
Take one down, pass it around,
5 bottles of beer on the wall.

5 bottles of beer on the wall,
5 bottles of beer,
Take one down, pass it around,
4 bottles of beer on the wall.

4 bottles of beer on the wall,
4 bottles of beer,
Take one down, pass it around,
3 bottles of beer on the wall.

3 bottles of beer on the wall,
3 bottles of beer,
Take one down, pass it around,
2 bottles of beer on the wall.

2 bottles of beer on the wall,
2 bottles of beer,
Take one down, pass it around,
1 bottles of beer on the wall.

1 bottles of beer on the wall,
1 bottles of beer,
Take one down, pass it around,
0 bottles of beer on the wall.
  
14. Primes
    
Input to lex
     
/*
 Simple prime number generator
 */
count = 1;
n = 1;
limit = 100;
while (n < limit) {
    k=3;
    p=1;
    n=n+2;
    while ((k*k<=n) && (p)) {
        p=n/k*k!=n;
        k=k+2;
    }
    if (p) {
        print(n, " is prime\n");
        count = count + 1;
    }
}
print("Total primes found: ", count, "\n");

.
.
.

Output from gen, input to vm
       
Datasize: 5 Strings: 3
" is prime\n"
"Total primes found: "
"\n"
   0 push  1
   5 store [0]
  10 push  1
  15 store [1]
  20 push  100
  25 store [2]
  30 fetch [1]
  35 fetch [2]
  40 lt
  41 jz     (160) 202
  46 push  3
  51 store [3]
  56 push  1
  61 store [4]
  66 fetch [1]
  71 push  2
  76 add
  77 store [1]
  82 fetch [3]
  87 fetch [3]
  92 mul
  93 fetch [1]
  98 le
  99 fetch [4]
 104 and
 105 jz     (53) 159
 110 fetch [1]
 115 fetch [3]
 120 div
 121 fetch [3]
 126 mul
 127 fetch [1]
 132 ne
 133 store [4]
 138 fetch [3]
 143 push  2
 148 add
 149 store [3]
 154 jmp    (-73) 82
 159 fetch [4]
 164 jz     (32) 197
 169 fetch [1]
 174 prti
 175 push  0
 180 prts
 181 fetch [0]
 186 push  1
 191 add
 192 store [0]
 197 jmp    (-168) 30
 202 push  1
 207 prts
 208 fetch [0]
 213 prti
 214 push  2
 219 prts
 220 halt 
 
Output
     
3 is prime

5 is prime

7 is prime

11 is prime

13 is prime

17 is prime

19 is prime

23 is prime

29 is prime

31 is prime

37 is prime

41 is prime

43 is prime

47 is prime

53 is prime

59 is prime

61 is prime

67 is prime

71 is prime

73 is prime

79 is prime

83 is prime

89 is prime

97 is prime
   
101 is prime
    
Total primes found: 26
      
15. ASCII Mandlebrot
    
Input to lex
      
{
/*
 This is an integer ascii Mandelbrot generator
 */
    left_edge   = -420;
    right_edge  =  300;
    top_edge    =  300;
    bottom_edge = -300;
    x_step      =    7;
    y_step      =   15;

    max_iter    =  200;

    y0 = top_edge;
    while (y0 > bottom_edge) {
        x0 = left_edge;
        while (x0 < right_edge) {
            y = 0;
            x = 0;
            the_char = ' ';
            i = 0;
            while (i < max_iter) {
                x_x = (x * x) / 200;
                y_y = (y * y) / 200;
                if (x_x + y_y > 800 ) {
                    the_char = '0' + i;
                    if (i > 9) {
                        the_char = '@';
                    }
                    i = max_iter;
                }
                y = x * y / 100 + y0;
                x = x_x - y_y + x0;
                i = i + 1;
            }
            putc(the_char);
            x0 = x0 + x_step;
        }
        putc('\n');
        y0 = y0 - y_step;
    }
}

.
.
.

Output from gen, input to vm
       
Datasize: 15 Strings: 0
    0 push  420
    5 neg
    6 store [0]
   11 push  300
   16 store [1]
   21 push  300
   26 store [2]
   31 push  300
   36 neg
   37 store [3]
   42 push  7
   47 store [4]
   52 push  15
   57 store [5]
   62 push  200
   67 store [6]
   72 fetch [2]
   77 store [7]
   82 fetch [7]
   87 fetch [3]
   92 gt
   93 jz     (329) 423
   98 fetch [0]
  103 store [8]
  108 fetch [8]
  113 fetch [1]
  118 lt
  119 jz     (276) 396
  124 push  0
  129 store [9]
  134 push  0
  139 store [10]
  144 push  32
  149 store [11]
  154 push  0
  159 store [12]
  164 fetch [12]
  169 fetch [6]
  174 lt
  175 jz     (193) 369
  180 fetch [10]
  185 fetch [10]
  190 mul
  191 push  200
  196 div
  197 store [13]
  202 fetch [9]
  207 fetch [9]
  212 mul
  213 push  200
  218 div
  219 store [14]
  224 fetch [13]
  229 fetch [14]
  234 add
  235 push  800
  240 gt
  241 jz     (56) 298
  246 push  48
  251 fetch [12]
  256 add
  257 store [11]
  262 fetch [12]
  267 push  9
  272 gt
  273 jz     (14) 288
  278 push  64
  283 store [11]
  288 fetch [6]
  293 store [12]
  298 fetch [10]
  303 fetch [9]
  308 mul
  309 push  100
  314 div
  315 fetch [7]
  320 add
  321 store [9]
  326 fetch [13]
  331 fetch [14]
  336 sub
  337 fetch [8]
  342 add
  343 store [10]
  348 fetch [12]
  353 push  1
  358 add
  359 store [12]
  364 jmp    (-201) 164
  369 fetch [11]
  374 prtc
  375 fetch [8]
  380 fetch [4]
  385 add
  386 store [8]
  391 jmp    (-284) 108
  396 push  10
  401 prtc
  402 fetch [7]
  407 fetch [5]
  412 sub
  413 store [7]
  418 jmp    (-337) 82
  423 halt
  
Output

1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
111                                                                 @@876555444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211



 


  


