module gl_f90_mod

    use opengl_gl
    use opengl_glu
    use opengl_glut
    implicit none
    
    integer(GLint) :: wW = 1000, wH = 1000
    integer(glcint) window
    
    contains
    
    subroutine output(x, y, text, scale)
    
        integer :: i
        real(glfloat) x,y
        character(len=*) text
        integer(glcint) p
        real(glfloat) scale
        
        call glTranslatef(x, y, 0.0_glfloat)
        i=1
        do while (i <= len(text))
            p = ichar(text(i:i))
            if (i < len(text)+1 .and. text(i:i+1) == '\n') then
                call glTranslatef(-(i+1)*scale, -scale*1.5, 0.0_glfloat)
                i = i+2
                cycle
            end if
            i = i+1
            call glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN, p)
        end do
    
    end subroutine output
    
    subroutine display() bind(c)
        call glViewport(0_GLint,0_GLint,wW,wH)
        call glMatrixMode(GL_PROJECTION)
        call glLoadIdentity()
        call gluOrtho2D(0.0_gldouble,wW+0.0_gldouble,0.0_gldouble,wH+0.0_gldouble)
        call glClearColor(1.0,1.0,1.0,1.0)
        call glClear(GL_COLOR_BUFFER_BIT)
        call glColor3f(1.0,0.0,0.0)
        
        !> enter some fancy draw code here!
        !> call drawSomeCoolStuff(...)

        call glscalef(.1,.1,.1)
        call output(0.01*wW/0.1, wH*0.95/0.1, "This is text\n Esc to quit",10/0.1)
        call glFlush()
        call glutPostRedisplay()
    end subroutine display
    
    subroutine key(ikey, x, y) bind(c)
        INTEGER(GLbyte), VALUE :: ikey
        INTEGER(GLint), VALUE :: x, y
        print *, ikey
        select case(ikey)
        case (27)
          stop
        end select
    end subroutine
    
    subroutine init()
        integer(GLenum) type
    
        call glutInitWindowSize(wW,wH)
        call glutInit()
        type = GLUT_RGB
        type = ior(type,GLUT_SINGLE)
        call glutInitDisplayMode(type)
        window = glutCreateWindow("GL and Fortran!")
        call glutDisplayFunc(display)
        call glutKeyboardFunc(key)
           
        call glutMainLoop()
    
    end subroutine init
    
    end module gl_f90_mod
    
    program main
        use opengl_gl
        use opengl_glut
        use gl_f90_mod
    
        call init()
    end program main
    