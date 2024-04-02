! Copyright 2015 Fyodorov S. A.

module Group_IO
   use Environment

   implicit none
   integer, parameter :: STUD_AMOUNT   = 5
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: MARKS_AMOUNT  = 4
   
   ! Чтение списка класса: фамилии, полы и оценки.
   pure subroutine Read_class_list(Input_file, Surnames, Genders, Marks)
      
      character(SURNAME_LEN, kind=CH_), intent(in) :: Surnames(STUD_AMOUNT) = "" 
      character(kind=CH_), intent(in)              :: Genders(STUD_AMOUNT) = ""
      integer,  intent(in)                         :: Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
      character(*), intent(in)                        :: Data_File
      
      
      type(student)                 Group(STUD_AMOUNT)

   character(SURNAME_LEN, kind=CH_), allocatable   :: Surnames(STUD_AMOUNT) = "" 
   character(kind=CH_),              allocatable   :: Genders(STUD_AMOUNT) = ""
   integer,                          allocatable   :: Marks(STUD_AMOUNT, MARKS_AMOUNT) = 0
      integer In, IO, recl
      
      recl = ((SURNAME_LEN + INITIALS_LEN + 1)*CH_ + MARKS_AMOUNT*I_ + R_) * STUD_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(in)  :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(3(a, 1x), ' // MARKS_AMOUNT // 'i1, f5.2)'
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
end module Group_IO 
