REM Maxwell Goldberg 5/16/2022
REM Matrix Multiplication Simulation


displayMainMenu
WHILE key$ <> "4"

    IF key$ = "1" THEN
        runMatMult
        displayMainMenu
    ELSEIF key$ = "2" THEN
        displayInfo
        displayMainMenu
    ELSEIF key$ = "3" THEN
        displayHelp
        displayMainMenu
    END IF

    key$ = INKEY$
WEND

END


DIM SHARED win(4)
DIM SHARED vecs(3, 2) ' first row: vector, second row: i-hat, third row: j-hat
DIM SHARED colors(3)
SUB runMatMult
    SCREEN 12

    ' DIM SHARED win(4)
    win(0) = -10 'left
    win(1) = -10 'bottom
    win(2) = 10 'right
    win(3) = 10 'top

    ar = 0.75
    WINDOW (win(0), win(1) * ar)-(win(2), win(3) * ar)


    ' DIM SHARED vecs(3, 2) ' first row: vector, second row: i-hat, third row: j-hat
    initializeVecs
    ' DIM SHARED colors(3)
    colors(0) = 10
    colors(1) = 11
    colors(2) = 12

    ' initialize everything
    cur_vec = 0
    vec$ = "V"
    cur_comp = 0
    comp$ = "X"
    min = win(0)
    max = win(2)
    dx = 0.1
    dy = 0.1
    d = dx
    transformed = 0

    in$ = "V"
    plotGrid 15
    plotVecs 1
    printMenu vec$, comp$

    WHILE UCASE$(in$) <> "E"

        in$ = UCASE$(INKEY$)

        IF transformed = 0 THEN
            IF in$ = "V" THEN
                cur_vec = 0
                vec$ = "V"
                printMenu vec$, comp$
            ELSEIF in$ = "I" THEN
                cur_vec = 1
                vec$ = "I"
                printMenu vec$, comp$
            ELSEIF in$ = "J" THEN
                cur_vec = 2
                vec$ = "J"
                printMenu vec$, comp$
            END IF

            IF in$ = "X" THEN
                cur_comp = 0
                min = win(0)
                max = win(2)
                d = dx
                comp$ = "X"
                printMenu vec$, comp$
            ELSEIF in$ = "Y" THEN
                cur_comp = 1
                min = win(1)
                max = win(3)
                d = dy
                comp$ = "Y"
                printMenu vec$, comp$
            END IF


            IF in$ = "+" AND max > vecs(cur_vec, cur_comp) THEN
                plotVecs 0
                vecs(cur_vec, cur_comp) = vecs(cur_vec, cur_comp) + dx
                plotGrid 15
                plotVecs 1
                printMenu vec$, comp$
            ELSEIF in$ = "-" AND min < vecs(cur_vec, cur_comp) THEN
                plotVecs 0
                vecs(cur_vec, cur_comp) = vecs(cur_vec, cur_comp) - dx
                plotGrid 15
                plotVecs 1
                printMenu vec$, comp$
            END IF

            IF in$ = "G" THEN
                v0x = vecs(0, 0)
                v0y = vecs(0, 1)
                matMult
                vecs(0, 0) = v0x
                vecs(0, 1) = v0y
                transformed = 1
            END IF

        END IF

        IF in$ = "R" OR in$ = "S" THEN
            IF in$ = "R" THEN
                initializeVecs
            END IF
            CLS
            plotGrid 15
            plotVecs 1
            cur_vec = 0
            vec$ = "V"
            cur_comp = 0
            comp$ = "X"
            min = win(0)
            max = win(2)
            d = dx
            transformed = 0
            printMenu vec$, comp$
        END IF

    WEND

END SUB


SUB initializeVecs ()
    vecs(0, 0) = 1
    vecs(0, 1) = 1
    vecs(1, 0) = 1
    vecs(1, 1) = 0
    vecs(2, 0) = 0
    vecs(2, 1) = 1
END SUB


SUB matMult ()

    plotGrid 15

    DIM mat(2, 2)
    ' e1
    mat(0, 0) = vecs(1, 0)
    mat(1, 0) = vecs(1, 1)
    ' e2
    mat(0, 1) = vecs(2, 0)
    mat(1, 1) = vecs(2, 1)

    dt = 0.001
    DIM v0(2)
    v0(0) = vecs(0, 0)
    v0(1) = vecs(0, 1)
    LINE (0, 0)-(x0, y0)

    CLS

    DIM ov(2), uh(2), uv(2), ouh(2), ouv(2)

    FOR t = 0 TO 1 STEP dt

        FOR i = 0 TO 1
            ouh(i) = uh(i)
            ouv(i) = uv(i)
            ov(i) = vecs(0, i)
        NEXT i
        _DELAY 0.005
        plotGrid 0
        FOR c = -10 TO 10 STEP 1
            span ouh(0), ouh(1), c * ouv(0), c * ouv(1), 0
            span ouv(0), ouv(1), c * ouh(0), c * ouh(1), 0
        NEXT c
        plotVecs 0
        'arrow 0, 0, ov(0), ov(1), 0

        plotGrid 8
        CIRCLE (0, 0), 0.2
        PAINT (0, 0)

        uh(0) = mat(0, 0) * t - t + 1
        uh(1) = mat(1, 0) * t

        uv(0) = mat(0, 1) * t
        uv(1) = mat(1, 1) * t - t + 1

        FOR c = -10 TO 10 STEP 1
            ' horizontal lines = (1,0)' + c * (0,1)'
            span uh(0), uh(1), c * uv(0), c * uv(1), 15

            ' vertical lines = (0,1) ' + c * (1,0)'
            span uv(0), uv(1), c * uh(0), c * uh(1), 15
        NEXT c

        vecs(0, 0) = v0(0) * (mat(0, 0) * t - t + 1) + v0(1) * t * mat(0, 1)
        vecs(0, 1) = v0(1) * (mat(1, 1) * t - t + 1) + v0(0) * t * mat(1, 0)
        plotVecs 1
        printMenu "", ""
        'arrow 0, 0, vecs(0, 0), vecs(0, 1), 11

    NEXT t

END SUB


SUB plotGrid (col)
    CIRCLE (0, 0), 0.2
    PAINT (0, 0)
    FOR c = -10 TO 10 STEP 1
        span 1, 0, 0, c, col
        span 0, 1, c, 0, col
    NEXT c
END SUB


SUB displayMainMenu ()
    SCREEN 0
    CLS
    COLOR 7: LOCATE 3, 1: PRINT "                         Matrix Multiplication Animator"
    PRINT "                          Created by: Maxwell Goldberg"
    LOCATE 9, 2: PRINT "Input one of the following numbers:"
    LOCATE 12, 2: PRINT "1) Run an animation of matrix multiplication."
    LOCATE 14, 2: PRINT "2) See info about matrix multiplication and what this program illustrates."
    LOCATE 16, 2: PRINT "3) See help for instructions of how to run program."
    LOCATE 18, 2: PRINT "4) Quit the program."
END SUB



SUB clearMenu
    LOCATE 1, 1: PRINT "                                                                                "
END SUB


SUB printMenu (vec$, comp$)
    clearMenu
    LOCATE 1, 1

    menuText vec$, comp$
END SUB


SUB menuText (vec$, comp$)
    SELECT CASE vec$
        CASE "V"
            COLOR colors(0): PRINT "Vector";: COLOR 15: PRINT "  I-hat  J-hat      ";:
        CASE "I"
            COLOR 15: PRINT "Vector  ";: COLOR colors(1): PRINT "I-hat";: COLOR 15: PRINT "  J-hat      ";:
        CASE "J"
            COLOR 15: PRINT "Vector  ";: COLOR 15: PRINT "I-hat";: COLOR colors(2): PRINT "  J-hat      ";:
        CASE ""
            COLOR 15: PRINT "Vector  I-hat  J-hat      ";:
    END SELECT

    SELECT CASE comp$
        CASE "X"
            COLOR 14: PRINT "X";: COLOR 15: PRINT "  Y      ";:
        CASE "Y"
            COLOR 15: PRINT "X";: COLOR 14: PRINT "  Y      ";:
        CASE ""
            COLOR 15: PRINT "X  Y      ";:
    END SELECT

    COLOR 15: PRINT "Go  reStart  Reset  Exit"
END SUB


SUB displayHelp ()
    CLS
    COLOR 7: LOCATE 2, 1: PRINT "                                      Help"

    LOCATE 4, 2: PRINT "The following menu displays in the program:"

    LOCATE 6, 2: menuText "", ""

    COLOR 7: LOCATE 8, 2: PRINT "This menu allows you to adjust the vectors and run the program."

    LOCATE 10, 2: PRINT "Press I to select i-hat."
    PRINT " Press J to select j-hat."
    PRINT " Press V to select the third vector."
    PRINT
    PRINT " Press X to select the x-component of the selected vector."
    PRINT " Press Y to select the y-component of the selected vector."
    PRINT
    PRINT " Press + to increase the selected component."
    PRINT " Press - to decrease the selected component."
    PRINT
    PRINT " Press G to run the program."
    PRINT " Press S to restart the program with your previously set vectors."
    PRINT " Press R to restart the program and reset vectors back to original values."
    PRINT " Press E to exit back to the main menu."
    PRINT
    PRINT " Press any key to return to the main menu."
    SLEEP
END SUB

SUB displayInfo ()
    CLS
    SCREEN 0
    COLOR 7: LOCATE 1, 1: PRINT "                                   Description"
    PRINT
    COLOR 7: PRINT "This program represents what matrix multiplication visually does.  As you may "
    PRINT "know, in the xy-plane, we can write vectors as (x, y).  With this notation, we "
    PRINT "implicitly assume that this vector is equal to";
    PRINT " x * ";: COLOR 3: PRINT "i-hat";: COLOR 7: PRINT " + y * ";: COLOR 4: PRINT "j-hat";
    COLOR 7: PRINT ", where ";: COLOR 3: PRINT "i-hat";: COLOR 7: PRINT " and ";: COLOR 4: PRINT "j-hat";: COLOR 7: PRINT " are the unit vectors in the x and y directions respectively."
    PRINT
    PRINT "In this program, we multiply a vector in the xy-plane by a 2x2 matrix.  Note"
    PRINT "that the vector is written as a 2x1 'column vector,' which outputs a new 2x1"
    PRINT "column vector.  A 2x2 matrix can be seen as a table of two columns vectors. The"
    PRINT "intuition behind matrix multiplication comes from these columns.  The first"
    PRINT "column is the vector that ";
    COLOR 3: PRINT "i-hat";: COLOR 7: PRINT " is transformed to, and the second column is the"
    PRINT "vector that ";: COLOR 4: PRINT "j-hat";: COLOR 7: PRINT " is transformed to.  ";
    PRINT "So, if these two columns are ";: COLOR 11: PRINT "i-hat'";: COLOR 7: PRINT " and ";: COLOR 12: PRINT "j-hat'";: COLOR 7: PRINT "and recalling that a vector (x,y) = ";
    PRINT "x * ";: COLOR 3: PRINT "i-hat";: COLOR 7: PRINT " + y * ";: COLOR 4: PRINT "j-hat";: COLOR 7: PRINT ", then matrix"
    PRINT "multiplication gives us the following:"
    PRINT
    PRINT "matrix * [ x * ";: COLOR 3: PRINT "i-hat";: COLOR 7: PRINT " + y * ";: COLOR 4: PRINT "j-hat";: COLOR 7: PRINT " ] = ";
    PRINT "[ x * ";: COLOR 11: PRINT "i-hat'";: COLOR 7: PRINT " + y * ";: COLOR 12: PRINT "j-hat'";: COLOR 7: PRINT " ]"
    PRINT
    PRINT "This program allows you to adjust the columns vectors in the matrix along with a"
    PRINT "third test vector to observe get multiplied.  The program then performs matrix"
    PRINT "multiplication on every lattice point in the plane, which is the reasoning of"
    PRINT "the stretching of the grid."
    PRINT
    PRINT "Press any key to return back to the main menu."
    SLEEP
END SUB

SUB plotVecs (c)
    IF c = 0 THEN
        FOR i = 0 TO 2
            arrow 0, 0, vecs(i, 0), vecs(i, 1), 0
        NEXT i
    ELSE
        FOR i = 0 TO 2
            arrow 0, 0, vecs(i, 0), vecs(i, 1), colors(i)
        NEXT i
    END IF
END SUB


SUB arrow (ax, ay, bx, by, c)
    LINE (ax, ay)-(bx, by), c
    ' Making lines on arrow head make angle s.t. opposite side half that of a leg of a 45-45-90 triangle & is 1/4 the size of original vector
    ' Note that arcsin(sin(45)/2) approx = 20.7 deg, and tan(20.7 deg) approx = 0.377964473, so will use this value instead of calculating each time
    headx = (bx - ax) * 0.377964473 / 4 ' Want line for arrow head to have projection onto vector to be 1/4 the length, this gives us tan*norm/4
    heady = (by - ay) * 0.377964473 / 4 ' Note that to produce this length, we divide by norm to get unit vector and multiply by tan*norm/4, so just multiply by tan/4
    ' These are the coordinates of the orthogonal vectors which should be added to the vector at 3/4 of its length
    ' So, let's add these to ax + (bx-ax) * 3/4 and same for y-comp
    ' Note: 90 deg cc rotation is (x,y) -> (-y,x), 90 deg c rotation is (x,y) -> (y,-x) = -(-y,x) (makes sense since 180 deg rotation is (x,y) -> -(x,y))
    LINE (ax, ay)-(headx, heady), 1
    LINE (bx, by)-(ax + (bx - ax) * 0.75 - heady, ay + (by - ay) * 0.75 + headx), c
    LINE (bx, by)-(ax + (bx - ax) * 0.75 + heady, ay + (by - ay) * 0.75 - headx), c

END SUB


SUB span (v1, v2, o1, o2, c) ' can't pass an array as parameter, can't return arrays
    LINE (-(win(0) ^ 2 + win(1) ^ 2) * v1 + o1, -(win(0) ^ 2 + win(1) ^ 2) * v2 + o2)-((win(2) ^ 2 + win(3) ^ 2) * v1 + o1, (win(2) ^ 2 + win(3) ^ 2) * v2 + o2), c
END SUB


