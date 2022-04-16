'©Vitaliy Karavaev.
'This program are complited by KVG

Imports System.Drawing.Drawing2D
Imports System.Drawing.Imaging


Public Class Form1




    ' The Bitmap and Graphics objects we will draw on.
    Private m_Bitmap As Bitmap
    Private m_Graphics As Graphics


    Private m_Drawing As Boolean




    Private m_X As Integer
    Private m_Y As Integer



    ReadOnly penLine As New Pen(Color.Black)

    ReadOnly Brush1 As New SolidBrush(Color.Black)

    'options of drawing

    Private M_D1 As New Point

    Private MD1, MD2, MD3 As New Point

    Private MD_1, MD_2, MD_3, MD_4 As New Point


    Private _MD_1, _MD_2, _MD_3, _MD_4, _MD_5, _MD_6, _MD_7 As New Point


    Private _MD_1_, _MD_2_, _MD_3_, _MD_4_, _MD_5_, _MD_6_, _MD_7_ As New Point


    Private MD_1_, MD_2_, MD_3_, MD_4_, MD_5_, MD_6_, MD_7_, MD_8_, MD_9_, MD_10_ As Point


    Private ReadOnly m_Points0(3) As Point


    Private ReadOnly m_Points2(7) As Point



    Private ReadOnly m_Points1(10) As Point



    Private AllPoints2 As Integer = -1 'for you point, inspite the first curve may be from 00.


    Private AllPoints() As Point
    Private PointLim As Integer
    Dim scanPoint As Point
    Const PointSize As Integer = 1



    'Besier

    Dim startP, PointC1, PointC2, EndP As New Point


    Private ReadOnly m_Points(4) As Point


    Private AllPoints1 As Integer = -1 'for you point, inspite the first Besier may be from 00.


    Private I As Integer


    Private currentImage As Image = Nothing



    Private a As Double = 0.1


    'объявление и декларация переменной строки для текстурирированной браши.
    'string variable for texture Brash
    Private str As String

    ReadOnly SelectionArea As New StretchableL(Me) ' Class StretchableL
#Region " Class StretchableL"


    Public Class StretchableL
        ' Переменные уровня класса
        Private BasePoint As Point
        Private ExtentPoint As Point
        Private CurrentState As StretchableLState


        Private ReadOnly BaseControl As Control
        '   Текущее состояние рисования
        Public Enum StretchableLState
            Inactive
            FirstTime
            Active
        End Enum

        Public Sub New(ByVal useControl As Control)
            ' Конструктор с одним параметром
            BaseControl = useControl

        End Sub
        Public ReadOnly Property Rectangle() As Rectangle
            Get
                'Возвращаем границы области, указанной резиновой линией
                Dim result As Rectangle
                'Отсчет координат с левого верхнего угла вниз и направо.
                result.X = If(BasePoint.X < ExtentPoint.X, BasePoint.X, ExtentPoint.X)
                result.Y = If(BasePoint.Y < ExtentPoint.Y, BasePoint.Y, ExtentPoint.Y)
                result.Width = Math.Abs(ExtentPoint.X - BasePoint.X)
                result.Height = Math.Abs(ExtentPoint.Y - BasePoint.Y)
                Return result
            End Get
        End Property
        Public Sub Start(ByVal x As Integer, ByVal y As Integer)
            ' начинаем рисовать
            ' прорисовывать первое изображение линии, необходимо 
            'вызваать метод Stretch().
            BasePoint.X = x
            BasePoint.Y = y
            ExtentPoint.X = x
            ExtentPoint.Y = y
            Normalize(BasePoint)
            CurrentState = StretchableLState.FirstTime
        End Sub
        Public Sub Stretch(ByVal x As Integer, ByVal y As Integer)
            'Меняем размер линии
            Dim NewPoint As Point
            'Подготавливаем новую точку для растяжения
            NewPoint.X = x
            NewPoint.Y = y
            Normalize(NewPoint)
            Select Case CurrentState
                Case StretchableLState.Inactive
                    'Линия не активна
                    Return
                Case StretchableLState.FirstTime
                    'рисуем начальную линию
                    ExtentPoint = NewPoint
                    DrawTheRectangle()
                    CurrentState = StretchableLState.Active
                Case StretchableLState.Active
                    'удаляем предыдущуб линию
                    'потом рисуем новую
                    DrawTheRectangle()
                    ExtentPoint = NewPoint
                    DrawTheRectangle()
            End Select
        End Sub
        Public Sub Finish()
            'прекращаем рисовать линию
            DrawTheRectangle()
            CurrentState = 0
        End Sub
        Private Sub Normalize(ByRef whichPoint As Point)
            ' не даем линии покидать область видимости
            If (whichPoint.X < 0) Then whichPoint.X = 0
            If (whichPoint.X >= BaseControl.ClientSize.Width) Then whichPoint.X = BaseControl.ClientSize.Width - 1

            If (whichPoint.Y < 0) Then whichPoint.Y = 0
            If (whichPoint.Y >= BaseControl.ClientSize.Height) Then whichPoint.Y = BaseControl.ClientSize.Height - 1

        End Sub
        Private Sub DrawTheRectangle()
            'Рисуем прямоугольник на поверхности формы
            ' или элемента управления
            Dim drawArea As Rectangle
            Dim screenStart, screenEnd As Point
            'получаем квадрат площадь линии
            screenStart = BaseControl.PointToScreen(BasePoint)
            screenEnd = BaseControl.PointToScreen(ExtentPoint)
            drawArea.X = screenStart.X
            drawArea.Y = screenStart.Y
            drawArea.Width = (screenEnd.X - screenStart.X)
            drawArea.Height = (screenEnd.Y - screenStart.Y)
            ' рисуем применяя стиль
            ControlPaint.DrawReversibleFrame(drawArea, Color.Transparent, FrameStyle.Dashed)

        End Sub
    End Class
#End Region


#Region "For Form Load"
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Try


            Dim SpSCR As New SplashScreen1

            SpSCR.Show()


            MakeNewBitmap()


            ToolStripMenuItem2.Enabled = False
            ToolStripMenuItem3.Enabled = False
            ToolStripMenuItem4.Enabled = False
            HorizontalToolStripMenuItem.Enabled = False
            VerticalToolStripMenuItem.Enabled = False
            Button3.Enabled = False
            Button4.Enabled = False
            Button5.Enabled = False
            Button6.Enabled = False
            Button7.Enabled = False


            RadioButton15.Checked = True




            TextureBrushPictureToolStripMenuItem.Enabled = False

            'RemoveHandler PictureBox1.MouseMove, AddressOf PictureBox1_MouseMove
            'you may just naw create the object of graphics or RemoveHandler MouseMove to the moment of such creation 
            'so that user only after the activation of the option New, can be able to draw



            'Text


            ComboBox1.SelectedItem = "Times New Roman"

            Dim i1 As Integer
            For i1 = 1 To 300


                ToolStripComboBox1.Items.Add(i1)

            Next
            ToolStripComboBox1.SelectedItem = 1

            Dim i15 As Integer
            For i15 = 1 To 300


                ComboBox2.Items.Add(i15)

            Next
            ComboBox2.SelectedItem = 12

            Dim i117 As Integer
            For i117 = 1 To 167
                'Если цвет не "Transparent":
                If i117 <> 27 Then _
            ComboBox3.Items.Add(Color.FromKnownColor((CType(i117, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox3.Sorted = True
            ' выбор пункта
            ComboBox3.SelectedItem = Color.Black


            'System of coorditate

            Dim i2 As Integer
            For i2 = 1 To 300


                ComboBox4.Items.Add(i2)

            Next
            ComboBox4.SelectedItem = 10

            Dim i3 As Integer
            For i3 = 1 To 300


                ComboBox5.Items.Add(i3)

            Next
            ComboBox5.SelectedItem = 10


            Dim i4 As Integer
            For i4 = 1 To 200


                ComboBox6.Items.Add(i4)

            Next
            ComboBox6.SelectedItem = 100



            'Dash/Cap lines
            For Each cap As LineCap In System.Enum.GetValues(GetType(LineCap))
                ComboBox9.Items.Add(cap) 'startCap
                ComboBox9.SelectedItem = LineCap.Flat
                ComboBox10.Items.Add(cap) 'EndCap
                ComboBox10.SelectedItem = LineCap.Round
            Next

            ' Set up Dash Cap.
            For Each dash As DashCap In System.Enum.GetValues(GetType(DashCap))
                ComboBox11.Items.Add(dash) 'Dash Style
                ComboBox11.SelectedItem = DashCap.Flat
            Next

            ' Set up Line Join
            For Each style As DashStyle In System.Enum.GetValues(GetType(DashStyle))
                ComboBox12.Items.Add(style) 'Line Style
                ComboBox12.SelectedItem = DashStyle.Solid
            Next







            Dim i5 As Integer
            For i5 = 1 To 1000


                ComboBox7.Items.Add(i5)

            Next
            ComboBox7.SelectedItem = 150


            Dim i7 As Integer
            For i7 = 1 To 10


                ComboBox8.Items.Add(i7)

            Next
            ComboBox8.SelectedItem = 1






            For i1 = 1 To 300


                ToolStripComboBox1.Items.Add(i1)

            Next
            ToolStripComboBox1.SelectedItem = 1


            RadioButton1.Checked = True

            TextureBrushPictureToolStripMenuItem.Enabled = False

            Dim i111 As Integer
            For i111 = 1 To 167
                'Если цвет не "Transparent":
                If i111 <> 27 Then _
            ToolStripComboBox2.Items.Add(Color.FromKnownColor((CType(i111, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ToolStripComboBox2.Sorted = True
            ' выбор пункта
            ToolStripComboBox2.SelectedItem = Color.Fuchsia




            Dim i11 As Integer
            For i11 = 1 To 167
                'Если цвет не "Transparent":
                If i11 <> 27 Then _
            ToolStripComboBox3.Items.Add(Color.FromKnownColor((CType(i11, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ToolStripComboBox3.Sorted = True
            ' выбор пункта
            ToolStripComboBox3.SelectedItem = Color.Navy




            ' dr gr fill pl

            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox13.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox13.Sorted = True
            ' выбор пункта
            ComboBox13.SelectedItem = Color.Navy



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox14.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox14.Sorted = True
            ' выбор пункта
            ComboBox14.SelectedItem = Color.Green



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox15.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox15.Sorted = True
            ' выбор пункта
            ComboBox15.SelectedItem = Color.Red



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox16.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox16.Sorted = True
            ' выбор пункта
            ComboBox16.SelectedItem = Color.Crimson



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox17.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox17.Sorted = True
            ' выбор пункта
            ComboBox17.SelectedItem = Color.Lime



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox18.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox18.Sorted = True
            ' выбор пункта
            ComboBox18.SelectedItem = Color.DarkGreen



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox19.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox19.Sorted = True
            ' выбор пункта
            ComboBox19.SelectedItem = Color.Cyan



            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox20.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox20.Sorted = True
            ' выбор пункта
            ComboBox20.SelectedItem = Color.Coral


            For I = 1 To 167
                'Если цвет не "Transparent":
                If I <> 27 Then _
        ComboBox21.Items.Add(Color.FromKnownColor((CType(I, KnownColor))))
            Next
            'Сортировать записи в алфавитном порядке:
            ComboBox21.Sorted = True
            ' выбор пункта
            ComboBox21.SelectedItem = Color.HotPink






        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub
#End Region

    ' Make a new Bitmap to fit the canvas.
 Private Sub MakeNewBitmap()
        ' Get the drawing surface's size.
        Dim wid As Integer = PictureBox1.ClientSize.Width
        Dim hgt As Integer = PictureBox1.ClientSize.Height

        ' Make a Bitmap and Graphics to fit.
        m_Bitmap = New Bitmap(wid, hgt)
        m_Graphics = Graphics.FromImage(m_Bitmap)

        ' Clear the drawing area.
        m_Graphics.Clear(Me.BackColor)

        ' Display the result.
        PictureBox1.Image = m_Bitmap


        m_Points1.ToList.Clear()

        scanPoint.ToString.ToList.Clear()


        PictureBox1.Refresh()

        PointsControlToolStripMenuItem.BackColor = Color.Red

        Delate()


        PictureBox1.Refresh()

    End Sub

    Function Delate() As Graphics
        Dim bmp As Bitmap
        bmp = New Bitmap(Width, Height)
        Dim G As Graphics
        BackgroundImage = bmp
        G = Graphics.FromImage(bmp)
        Return G
    End Function
#Region "Menu Options"


    Private Sub NewToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NewToolStripMenuItem.Click
        Try
            MakeNewBitmap()
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub OpenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenToolStripMenuItem.Click
        Try
            If OpenFileDialog1.ShowDialog = DialogResult.OK Then

                Dim s As String = OpenFileDialog1.FileName

                Try

                    m_Bitmap = New Bitmap(s)

                    currentImage = CType(m_Bitmap.Clone(), Image)


                    PictureBox1.Image = currentImage

                Catch ex As Exception
                    MessageBox.Show("File " + s + " has a wrong format.", "Error")
                    Return

                End Try

                Text = "DrawWithMousePB - " + s

                OpenFileDialog1.FileName = ""

            End If

            ToolStripMenuItem2.Enabled = True
            ToolStripMenuItem3.Enabled = True
            ToolStripMenuItem4.Enabled = True
            HorizontalToolStripMenuItem.Enabled = True
            VerticalToolStripMenuItem.Enabled = True
            Button3.Enabled = True
            Button4.Enabled = True
            Button5.Enabled = True
            Button6.Enabled = True
            Button7.Enabled = True


        Catch ex As Exception
            MessageBox.Show(ex.Message, "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try


    End Sub

    Private Sub SaveToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveToolStripMenuItem.Click
        Try
            Try
                If SaveFileDialog1.ShowDialog() = DialogResult.OK Then

                    Dim ContrastBmp = New Bitmap(PictureBox1.Image)


                    If (SaveFileDialog1.FileName.Contains(".bmp")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Bmp)

                    If (SaveFileDialog1.FileName.Contains(".jpeg")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Jpeg)

                    If (SaveFileDialog1.FileName.Contains(".gif")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Gif)

                    If (SaveFileDialog1.FileName.Contains(".png")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Png)

                End If




            Catch ex As Exception
                If CBool(MessageBox.Show("The file " + SaveFileDialog1.FileName + " has an inappropriate extension. Returning.")) Then
                    Return
                ElseIf CBool(MessageBox.Show("The result image saved under " + SaveFileDialog1.FileName)) Then

                Else MessageBox.Show(ex.Message, "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                End If

            End Try


        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        Try
            Close()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PenLineColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PenLineColorToolStripMenuItem.Click
        Try
            Dim cdlg As New ColorDialog()
            If cdlg.ShowDialog() = DialogResult.OK Then

                penLine.Color = cdlg.Color
            End If


        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub Brush1ColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles Brush1ColorToolStripMenuItem.Click
        Try
            Dim cdlg As New ColorDialog()
            If cdlg.ShowDialog() = DialogResult.OK Then

                Brush1.Color = cdlg.Color

            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub BackColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BackColorToolStripMenuItem.Click
        Try

            Dim cdlg As New ColorDialog()
            If cdlg.ShowDialog() = DialogResult.OK Then


                Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                    m_Graphics.Clear(cdlg.Color)

                    PictureBox1.Invalidate()


                End Using

            End If




        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub AllScreenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AllScreenToolStripMenuItem.Click
        Try
            ToolStrip1.Visible = False
            MenuStrip1.Visible = False
            FormBorderStyle = FormBorderStyle.None
            TabControl1.Visible = False

            PictureBox1.Dock = DockStyle.Fill

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub TextureBrushPictureToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles TextureBrushPictureToolStripMenuItem.Click
        Try
            If OpenFileDialog1.ShowDialog = DialogResult.OK Then
                'значеие переменной строки путь и имя файла для текстурированной браши
                Str = OpenFileDialog1.FileName.ToString

            End If
        Catch ex As Exception

        End Try
    End Sub




    Private Sub CopyToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyToolStripMenuItem.Click
        Try

            Clipboard.SetImage(PictureBox1.Image)
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PastToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PastToolStripMenuItem.Click
        Try
            If Clipboard.ContainsImage Then

                Dim bm1 As Image = Clipboard.GetImage()

                ' Make an associated Graphics object.
                Dim Gr As Graphics = CreateGraphics()

                Gr = Graphics.FromImage(bm1)


                ' Display the New bitmap in the PictureBox.
                PictureBox1.Image = bm1


            End If

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub CopyScreenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyScreenToolStripMenuItem.Click
        Try
            SendKeys.Send("%{PRTSC}")
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub InformationToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles InformationToolStripMenuItem.Click
        Try
            Dim Inf = New Form2()
            Inf.Show()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub AboutProgramToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutProgramToolStripMenuItem.Click
        Try

            Dim AB = New AboutBox1()
            AB.Show()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub ToolStripComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ToolStripComboBox1.SelectedIndexChanged
        Try
            penLine.Width = ToolStripComboBox1.SelectedIndex

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try
    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        Try
            Beep()
            Timer1.Enabled = Not Timer1.Enabled
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        Try
            Timer1.Enabled = False
            Opacity = 1
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Try
            ' прозрачность формы, клик увеличение прозрачности
            If Opacity <= 0 Or Opacity >= 1 Then
                ' Stop
                a = -a
            End If
            Opacity += a
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub


    Private Sub PointsControlToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PointsControlToolStripMenuItem.Click

        PointsControlToolStripMenuItem.BackColor = Color.Green

    End Sub

    Private Sub PointsControlRelaxToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PointsControlRelaxToolStripMenuItem.Click
        PointsControlToolStripMenuItem.BackColor = Color.Red

    End Sub



    Private Sub RotateFlip(ByVal degrees As Integer)
        Try


            If currentImage Is Nothing Then Exit Sub

            Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                Select Case degrees
                    Case 0
                        currentImage.RotateFlip(RotateFlipType.RotateNoneFlipX)
                    Case 1
                        currentImage.RotateFlip(RotateFlipType.RotateNoneFlipY)
                    Case 90
                        currentImage.RotateFlip(RotateFlipType.Rotate90FlipNone)
                        Exit Select
                    Case 180
                        currentImage.RotateFlip(RotateFlipType.Rotate180FlipNone)
                        Exit Select
                    Case 270
                        currentImage.RotateFlip(RotateFlipType.Rotate270FlipNone)
                    Case Else
                        Exit Select
                End Select

            End Using



        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try
    End Sub

    Private Sub ToolStripMenuItem7_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click, Button3.Click
        Try

            If ([String].IsNullOrEmpty(Text)) Then Exit Sub


            RotateFlip(90)

            PictureBox1.Refresh()

            '90 rotate
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try



    End Sub

    Private Sub ToolStripMenuItem8_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem3.Click, Button4.Click
        Try

            If ([String].IsNullOrEmpty(Text)) Then Exit Sub
            RotateFlip(180)

            PictureBox1.Refresh()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try


        '180 rotate
    End Sub

    Private Sub ToolStripMenuItem9_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem4.Click, Button5.Click
        Try

            If ([String].IsNullOrEmpty(Text)) Then Exit Sub
            '270 rotate
            RotateFlip(270)
            PictureBox1.Refresh()


        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try



    End Sub

    Private Sub HorizontalToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles HorizontalToolStripMenuItem.Click, Button6.Click
        Try
            If ([String].IsNullOrEmpty(Text)) Then Exit Sub
            RotateFlip(0)
            PictureBox1.Refresh()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try


        'flip Horizontal

    End Sub

    Private Sub VerticalToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles VerticalToolStripMenuItem.Click, Button7.Click
        'flip vertical

        Try
            If ([String].IsNullOrEmpty(Text)) Then Exit Sub
            RotateFlip(1)
            PictureBox1.Refresh()

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub


#End Region



#Region " Options of drawing"
    Private Sub PictureBox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        Try
            If e.Button = MouseButtons.Left Then



                If RadioButton2.Checked Then
                    m_Drawing = True
                    RadioButton2.BackColor = Color.Green
                    RadioButton2.ForeColor = Color.White

                    RadioButton2.Invalidate()



                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton3.Checked Then
                    m_Drawing = True
                    RadioButton3.BackColor = Color.Green
                    RadioButton3.ForeColor = Color.White

                    RadioButton3.Invalidate()



                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton4.Checked Then

                    m_Drawing = True

                    RadioButton4.BackColor = Color.Green
                    RadioButton4.ForeColor = Color.White
                    RadioButton4.Invalidate()


                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton5.Checked Then
                    m_Drawing = True

                    RadioButton5.BackColor = Color.Green
                    RadioButton5.ForeColor = Color.White
                    RadioButton5.Invalidate()

                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton6.Checked Then
                    m_Drawing = True

                    RadioButton6.BackColor = Color.Green
                    RadioButton6.ForeColor = Color.White
                    RadioButton6.Invalidate()


                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton7.Checked Then
                    m_Drawing = True

                    RadioButton7.BackColor = Color.Green
                    RadioButton7.ForeColor = Color.White
                    RadioButton7.Invalidate()



                ElseIf RadioButton8.Checked Then
                    m_Drawing = True

                    RadioButton8.BackColor = Color.Green
                    RadioButton8.ForeColor = Color.White
                    RadioButton8.Invalidate()


                ElseIf RadioButton9.Checked Then
                    m_Drawing = True

                    RadioButton9.BackColor = Color.Green
                    RadioButton9.ForeColor = Color.White
                    RadioButton9.Invalidate()

                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton10.Checked Then

                    m_Drawing = True

                    RadioButton10.BackColor = Color.Green
                    RadioButton10.ForeColor = Color.White
                    RadioButton10.Invalidate()

                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton11.Checked Then
                    m_Drawing = True
                    RadioButton11.BackColor = Color.Green
                    RadioButton11.ForeColor = Color.White
                    RadioButton11.Invalidate()

                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton12.Checked Then
                    m_Drawing = True

                    RadioButton12.BackColor = Color.Green
                    RadioButton12.ForeColor = Color.White
                    RadioButton12.Invalidate()


                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton13.Checked Then
                    m_Drawing = True

                    RadioButton13.BackColor = Color.Green
                    RadioButton13.ForeColor = Color.White
                    RadioButton13.Invalidate()


                    m_X = e.X
                    m_Y = e.Y


                ElseIf RadioButton14.Checked Then
                    m_Drawing = True

                    RadioButton14.BackColor = Color.Green
                    RadioButton14.ForeColor = Color.White
                    RadioButton14.Invalidate()

                    m_X = e.X
                    m_Y = e.Y




                ElseIf RadioButton17.Checked Then

                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then
                        If ([String].IsNullOrEmpty(str)) Then Exit Sub

                        m_Drawing = True


                        RadioButton17.BackColor = Color.Green
                        RadioButton17.ForeColor = Color.White
                        RadioButton17.Invalidate()


                        m_X = e.X
                        m_Y = e.Y
                    Else
                        Exit Sub
                    End If


                ElseIf RadioButton18.Checked Then
                    m_Drawing = True

                    RadioButton18.BackColor = Color.Green
                    RadioButton18.ForeColor = Color.White
                    RadioButton18.Invalidate()


                    PointLim = 0

                    ReDim AllPoints(PointLim)
                    AllPoints(PointLim).X = e.X
                    AllPoints(PointLim).Y = e.Y

                ElseIf RadioButton19.Checked Then

                    m_Drawing = True

                    RadioButton19.BackColor = Color.Green
                    RadioButton19.ForeColor = Color.White

                    RadioButton19.Invalidate()


                    PointLim = 0

                    ReDim AllPoints(PointLim)

                    AllPoints(PointLim).X = e.X
                    AllPoints(PointLim).Y = e.Y

                ElseIf RadioButton21.Checked Then
                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then
                        If ([String].IsNullOrEmpty(str)) Then Exit Sub

                        m_Drawing = True


                        RadioButton20.BackColor = Color.Green
                        RadioButton20.ForeColor = Color.White
                        RadioButton20.Invalidate()

                        m_X = e.X
                        m_Y = e.Y
                    Else
                        Exit Sub
                    End If

                ElseIf RadioButton27.Checked Then

                    m_Drawing = True
                    RadioButton27.BackColor = Color.Green
                    RadioButton27.ForeColor = Color.White
                    RadioButton27.Invalidate()

                    m_X = e.X
                    m_Y = e.Y

                ElseIf RadioButton29.Checked Then

                    m_Drawing = True
                    RadioButton29.BackColor = Color.Green
                    RadioButton29.ForeColor = Color.White
                    RadioButton29.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints1 = 4 Then
                            m_Points.ToList.Clear()
                            AllPoints1 = 0
                        Else
                            AllPoints1 += 1
                        End If


                        m_Points(AllPoints1).X = e.X
                        m_Points(AllPoints1).Y = e.Y


                        startP = m_Points(0)

                        PointC1 = m_Points(1)

                        PointC2 = m_Points(2)

                        EndP = m_Points(3)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints1 <= 4 Then

                                For Each scanPoint In m_Points



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint

                            Else
                                AllPoints1 += 1

                            End If


                        End If

                        PictureBox1.Refresh()

                    End Using
                ElseIf RadioButton30.Checked Then

                    m_Drawing = True
                    RadioButton30.BackColor = Color.Green
                    RadioButton30.ForeColor = Color.White
                    RadioButton30.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1 = m_Points2(0)
                        _MD_2 = m_Points2(1)
                        _MD_3 = m_Points2(2)
                        _MD_4 = m_Points2(3)
                        _MD_5 = m_Points2(4)
                        _MD_6 = m_Points2(5)
                        _MD_7 = m_Points2(6)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                    scanPoint.X - PointSize,
                                    scanPoint.Y - PointSize,
                                    PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If


                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton31.Checked Then

                    m_Drawing = True
                    RadioButton31.BackColor = Color.Green
                    RadioButton31.ForeColor = Color.White
                    RadioButton31.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1 = m_Points2(0)
                        _MD_2 = m_Points2(1)
                        _MD_3 = m_Points2(2)
                        _MD_4 = m_Points2(3)
                        _MD_5 = m_Points2(4)
                        _MD_6 = m_Points2(5)
                        _MD_7 = m_Points2(6)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                    scanPoint.X - PointSize,
                                    scanPoint.Y - PointSize,
                                    PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If


                    End Using


                    PictureBox1.Refresh()


                ElseIf RadioButton32.Checked = True Then

                    m_Drawing = True
                    RadioButton32.BackColor = Color.Green
                    RadioButton32.ForeColor = Color.White
                    RadioButton32.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 4 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        MD_1 = m_Points2(0)
                        MD_2 = m_Points2(1)
                        MD_3 = m_Points2(2)
                        MD_4 = m_Points2(3)


                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 4 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                    scanPoint.X - PointSize,
                                    scanPoint.Y - PointSize,
                                    PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()


                ElseIf RadioButton33.Checked = True Then

                    m_Drawing = True
                    RadioButton33.BackColor = Color.Green
                    RadioButton33.ForeColor = Color.White
                    RadioButton33.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 4 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        MD_1 = m_Points2(0)
                        MD_2 = m_Points2(1)
                        MD_3 = m_Points2(2)
                        MD_4 = m_Points2(3)


                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 4 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                    scanPoint.X - PointSize,
                                    scanPoint.Y - PointSize,
                                    PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If



                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton34.Checked = True Then

                    m_Drawing = True
                    RadioButton34.BackColor = Color.Green
                    RadioButton34.ForeColor = Color.White
                    RadioButton34.Invalidate()



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1_ = m_Points2(0)
                        _MD_2_ = m_Points2(1)
                        _MD_3_ = m_Points2(2)
                        _MD_4_ = m_Points2(3)
                        _MD_5_ = m_Points2(4)
                        _MD_6_ = m_Points2(5)
                        _MD_7_ = m_Points2(6)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint

                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()


                ElseIf RadioButton35.Checked = True Then

                    m_Drawing = True
                    RadioButton35.BackColor = Color.Green
                    RadioButton35.ForeColor = Color.White
                    RadioButton35.Invalidate()



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1_ = m_Points2(0)
                        _MD_2_ = m_Points2(1)
                        _MD_3_ = m_Points2(2)
                        _MD_4_ = m_Points2(3)
                        _MD_5_ = m_Points2(4)
                        _MD_6_ = m_Points2(5)
                        _MD_7_ = m_Points2(6)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint

                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()


                ElseIf RadioButton36.Checked = True Then

                    m_Drawing = True
                    RadioButton36.BackColor = Color.Green
                    RadioButton36.ForeColor = Color.White
                    RadioButton36.Invalidate()



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1_ = m_Points2(0)
                        _MD_2_ = m_Points2(1)
                        _MD_3_ = m_Points2(2)
                        _MD_4_ = m_Points2(3)
                        _MD_5_ = m_Points2(4)
                        _MD_6_ = m_Points2(5)
                        _MD_7_ = m_Points2(6)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint

                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()


                ElseIf RadioButton38.Checked = True Then

                    m_Drawing = True
                    RadioButton38.BackColor = Color.Green
                    RadioButton38.ForeColor = Color.White
                    RadioButton38.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 3 Then ' > 9
                            m_Points0.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points0(AllPoints2).X = e.X
                        m_Points0(AllPoints2).Y = e.Y


                        MD1 = m_Points0(0)
                        MD2 = m_Points0(1)
                        MD3 = m_Points0(2)


                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 3 Then

                                For Each scanPoint In m_Points0



                                    m_Graphics.FillEllipse(Brushes.Black,
                                    scanPoint.X - PointSize,
                                    scanPoint.Y - PointSize,
                                    PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton39.Checked = True Then

                    m_Drawing = True

                    RadioButton39.BackColor = Color.White
                    RadioButton39.ForeColor = Color.Black
                    RadioButton30.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 7 Then ' > 9
                            m_Points.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points2(AllPoints2).X = e.X
                        m_Points2(AllPoints2).Y = e.Y


                        _MD_1_ = m_Points2(0)
                        _MD_2_ = m_Points2(1)
                        _MD_3_ = m_Points2(2)
                        _MD_4_ = m_Points2(3)
                        _MD_5_ = m_Points2(4)
                        _MD_6_ = m_Points2(5)
                        _MD_7_ = m_Points2(6)


                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 7 Then

                                For Each scanPoint In m_Points2



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If

                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton40.Checked = True Then

                    m_Drawing = True

                    RadioButton40.BackColor = Color.White
                    RadioButton40.ForeColor = Color.Black
                    RadioButton40.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 10 Then ' > 9
                            m_Points1.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points1(AllPoints2).X = e.X
                        m_Points1(AllPoints2).Y = e.Y


                        MD_1_ = m_Points1(0)
                        MD_2_ = m_Points1(1)
                        MD_3_ = m_Points1(2)
                        MD_4_ = m_Points1(3)
                        MD_5_ = m_Points1(4)
                        MD_6_ = m_Points1(5)
                        MD_7_ = m_Points1(6)
                        MD_8_ = m_Points1(7)
                        MD_9_ = m_Points1(8)
                        MD_10_ = m_Points1(9)



                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 10 Then

                                For Each scanPoint In m_Points1



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If


                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton40.Checked = True Then

                    m_Drawing = True

                    RadioButton40.BackColor = Color.White
                    RadioButton40.ForeColor = Color.Black
                    RadioButton40.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints2 >= 10 Then
                            m_Points1.ToList.Clear()
                            AllPoints2 = 0
                        Else
                            AllPoints2 += 1
                        End If


                        m_Points1(AllPoints2).X = e.X
                        m_Points1(AllPoints2).Y = e.Y


                        MD_1_ = m_Points1(0)
                        MD_2_ = m_Points1(1)
                        MD_3_ = m_Points1(2)
                        MD_4_ = m_Points1(3)
                        MD_5_ = m_Points1(4)
                        MD_6_ = m_Points1(5)
                        MD_7_ = m_Points1(6)
                        MD_8_ = m_Points1(7)
                        MD_9_ = m_Points1(8)
                        MD_10_ = m_Points1(9)


                        If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                            If AllPoints2 <= 10 Then

                                For Each scanPoint In m_Points1



                                    m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                Next scanPoint
                                ' m_Points.ToList.Clear()
                            Else
                                AllPoints2 += 1

                            End If


                        End If


                    End Using


                    PictureBox1.Refresh()

                ElseIf RadioButton41.Checked Then
                    m_Drawing = True

                    RadioButton41.BackColor = Color.Green
                    RadioButton41.ForeColor = Color.White
                    RadioButton41.Invalidate()



                    SelectionArea.Start(e.X, e.Y)


                ElseIf RadioButton42.Checked Then
                    m_Drawing = True

                    RadioButton42.BackColor = Color.Green
                    RadioButton42.ForeColor = Color.White
                    RadioButton42.Invalidate()



                    SelectionArea.Start(e.X, e.Y)

                ElseIf RadioButton43.Checked Then
                    m_Drawing = True

                    RadioButton43.BackColor = Color.Green
                    RadioButton43.ForeColor = Color.White
                    RadioButton43.Invalidate()



                    SelectionArea.Start(e.X, e.Y)

                ElseIf RadioButton44.Checked Then

                    m_Drawing = True

                    RadioButton44.BackColor = Color.Green
                    RadioButton44.ForeColor = Color.White
                    RadioButton44.Invalidate()



                    SelectionArea.Start(e.X, e.Y)

                ElseIf RadioButton45.Checked Then
                    m_Drawing = True

                    RadioButton45.BackColor = Color.Green
                    RadioButton45.ForeColor = Color.White

                    RadioButton16.Invalidate()



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)
                        SelectionArea.Start(e.X, e.Y)

                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                    End Using

                End If

            End If

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        Try

            ToolStripTextBox2.Text = String.Format("X = {0} или {1}",
                                     MousePosition.X, e.X)
            ToolStripTextBox3.Text = String.Format("Y = {0} или {1}",
                                               MousePosition.Y, e.Y)

            If e.Button = MouseButtons.Left Then


                If RadioButton2.Checked Then
                    m_Drawing = True

                    RadioButton2.BackColor = Color.Green
                    RadioButton2.ForeColor = Color.White

                    RadioButton2.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawLine(penLine, m_X, m_Y, e.X, e.Y)


                        m_X = e.X
                        m_Y = e.Y


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()



                ElseIf RadioButton3.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton3.BackColor = Color.Green
                    RadioButton3.ForeColor = Color.White

                    RadioButton3.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawLine(penLine, m_X, m_Y, e.X, e.Y)

                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()



                ElseIf RadioButton4.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton4.BackColor = Color.Green
                    RadioButton4.ForeColor = Color.White

                    RadioButton4.Invalidate()



                ElseIf RadioButton5.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton5.BackColor = Color.Green
                    RadioButton5.ForeColor = Color.White

                    RadioButton5.Invalidate()


                ElseIf RadioButton6.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton6.BackColor = Color.Green
                    RadioButton6.ForeColor = Color.White

                    RadioButton6.Invalidate()



                ElseIf RadioButton7.Checked Then
                    m_Drawing = True

                    RadioButton7.BackColor = Color.Green
                    RadioButton7.ForeColor = Color.White

                    RadioButton7.Invalidate()



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias





                        Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown2.Value), CInt(NumericUpDown3.Value))

                        m_Graphics.FillEllipse(Brush1, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton8.Checked Then
                    m_Drawing = True
                    RadioButton8.BackColor = Color.Green
                    RadioButton8.ForeColor = Color.White

                    RadioButton8.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias




                        Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown2.Value), CInt(NumericUpDown3.Value))

                        m_Graphics.FillRectangle(Brush1, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton9.Checked Then
                    If Not m_Drawing Then Return

                    RadioButton9.BackColor = Color.Green
                    RadioButton9.ForeColor = Color.White

                    RadioButton9.Invalidate()


                ElseIf RadioButton10.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton10.BackColor = Color.Green
                    RadioButton10.ForeColor = Color.White

                    RadioButton10.Invalidate()



                ElseIf RadioButton11.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton11.BackColor = Color.Green
                    RadioButton11.ForeColor = Color.White

                    RadioButton11.Invalidate()



                ElseIf RadioButton12.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton12.BackColor = Color.Green
                    RadioButton12.ForeColor = Color.White

                    RadioButton12.Invalidate()



                ElseIf RadioButton13.Checked Then

                    m_Drawing = True

                    RadioButton13.BackColor = Color.Green
                    RadioButton13.ForeColor = Color.White

                    RadioButton13.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias




                        Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown2.Value), CInt(NumericUpDown3.Value))



                        Dim GrBrush As LinearGradientBrush

                        GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)



                        m_Graphics.FillRectangle(GrBrush, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton14.Checked Then

                    m_Drawing = True

                    RadioButton14.BackColor = Color.Green
                    RadioButton14.ForeColor = Color.White

                    RadioButton13.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias




                        Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown2.Value), CInt(NumericUpDown3.Value))



                        Dim GrBrush As LinearGradientBrush

                        GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)



                        m_Graphics.FillEllipse(GrBrush, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()



                ElseIf RadioButton44.Checked Then
                    m_Drawing = True

                    RadioButton44.BackColor = Color.Green
                    RadioButton44.ForeColor = Color.White


                    SelectionArea.Stretch(e.X, e.Y)


                ElseIf RadioButton17.Checked Then
                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then
                        If ([String].IsNullOrEmpty(str)) Then Exit Sub
                        m_Drawing = True


                        RadioButton17.BackColor = Color.Green
                        RadioButton17.ForeColor = Color.White

                        RadioButton17.Invalidate()



                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                            Dim texture As New TextureBrush(Image.FromFile(str))


                            Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown5.Value), CInt(NumericUpDown6.Value))


                            m_Graphics.FillEllipse(texture, Rect)


                        End Using

                        ' Display the result.
                        PictureBox1.Refresh()

                    Else
                        Exit Sub
                    End If


                ElseIf RadioButton18.Checked Then

                    If Not m_Drawing Then Exit Sub


                    RadioButton18.BackColor = Color.Green
                    RadioButton18.ForeColor = Color.White

                    RadioButton18.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        PointLim += 1
                        ReDim Preserve AllPoints(PointLim)
                        AllPoints(PointLim).X = e.X
                        AllPoints(PointLim).Y = e.Y

                        ' Dim demoPen As New Pen(penColor, ToolStripComboBox1.SelectedItem)
                        ' demoPen.Color = penColor

                        m_Graphics.DrawLine(penLine,
                AllPoints(PointLim - 1), AllPoints(PointLim))


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton19.Checked Then

                    If Not m_Drawing Then Exit Sub

                    RadioButton19.BackColor = Color.Green
                    RadioButton19.ForeColor = Color.White
                    RadioButton19.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        PointLim += 1
                        ReDim Preserve AllPoints(PointLim)
                        AllPoints(PointLim).X = e.X
                        AllPoints(PointLim).Y = e.Y

                        ' Dim demoPen As New Pen(penColor, ToolStripComboBox1.SelectedItem)
                        ' demoPen.Color = penColor

                        m_Graphics.DrawLine(penLine,
                AllPoints(PointLim - 1), AllPoints(PointLim))


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton21.Checked Then

                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then
                        If ([String].IsNullOrEmpty(str)) Then Exit Sub
                        If Not m_Drawing Then Exit Sub

                        RadioButton21.BackColor = Color.Green
                        RadioButton21.ForeColor = Color.White

                        RadioButton21.Invalidate()


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                            Dim Rect1 = New Rectangle(e.X, e.Y, CInt(NumericUpDown5.Value), CInt(NumericUpDown6.Value))



                            Dim GrBrush As LinearGradientBrush

                            GrBrush = New LinearGradientBrush(Rect1, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)


                            Dim texture As New TextureBrush(Image.FromFile(str))

                            Dim Rect = New Rectangle(e.X, e.Y, CInt(NumericUpDown2.Value), CInt(NumericUpDown3.Value))

                            'последовательность строк значима, the consequence of the line are meaningfull.

                            m_Graphics.FillRectangle(GrBrush, Rect1)


                            m_Graphics.FillEllipse(texture, Rect1)


                        End Using

                        ' Display the result.
                        PictureBox1.Refresh()
                    Else
                        Exit Sub
                    End If


                ElseIf RadioButton27.Checked Then

                    If Not m_Drawing Then Return

                    RadioButton27.BackColor = Color.Green
                    RadioButton27.ForeColor = Color.White
                    RadioButton27.Invalidate()

                ElseIf RadioButton29.Checked Then

                    If Not m_Drawing Then Return
                    RadioButton29.BackColor = Color.Green
                    RadioButton29.ForeColor = Color.White
                    RadioButton29.Invalidate()

                ElseIf RadioButton30.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton30.BackColor = Color.Green
                    RadioButton30.ForeColor = Color.White
                    RadioButton30.Invalidate()

                ElseIf RadioButton31.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton31.BackColor = Color.Green
                    RadioButton31.ForeColor = Color.White
                    RadioButton31.Invalidate()

                ElseIf RadioButton32.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton32.BackColor = Color.Green
                    RadioButton32.ForeColor = Color.White
                    RadioButton32.Invalidate()

                ElseIf RadioButton33.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton33.BackColor = Color.Green
                    RadioButton33.ForeColor = Color.White
                    RadioButton33.Invalidate()

                ElseIf RadioButton34.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton34.BackColor = Color.Green
                    RadioButton34.ForeColor = Color.White
                    RadioButton34.Invalidate()

                ElseIf RadioButton35.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton35.BackColor = Color.Green
                    RadioButton35.ForeColor = Color.White
                    RadioButton35.Invalidate()

                ElseIf RadioButton36.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton36.BackColor = Color.Green
                    RadioButton36.ForeColor = Color.White
                    RadioButton36.Invalidate()

                ElseIf RadioButton38.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton38.BackColor = Color.Green
                    RadioButton38.ForeColor = Color.White
                    RadioButton38.Invalidate()
                ElseIf RadioButton39.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton39.BackColor = Color.Green
                    RadioButton39.ForeColor = Color.White
                    RadioButton39.Invalidate()
                ElseIf RadioButton40.Checked Then
                    If Not m_Drawing Then Return
                    RadioButton40.BackColor = Color.Green
                    RadioButton40.ForeColor = Color.White
                    RadioButton40.Invalidate()

                ElseIf RadioButton41.Checked Then
                    m_Drawing = True

                    RadioButton41.BackColor = Color.Green
                    RadioButton41.ForeColor = Color.White


                    SelectionArea.Stretch(e.X, e.Y)
                    ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                ElseIf RadioButton42.Checked Then
                    m_Drawing = True

                    RadioButton42.BackColor = Color.Green
                    RadioButton42.ForeColor = Color.White


                    SelectionArea.Stretch(e.X, e.Y)

                    ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                ElseIf RadioButton43.Checked Then
                    m_Drawing = True

                    RadioButton43.BackColor = Color.Green
                    RadioButton43.ForeColor = Color.White


                    SelectionArea.Stretch(e.X, e.Y)

                    ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                ElseIf RadioButton44.Checked Then
                    m_Drawing = True

                    RadioButton44.BackColor = Color.Green
                    RadioButton44.ForeColor = Color.White


                    SelectionArea.Stretch(e.X, e.Y)

                    ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                ElseIf RadioButton45.Checked Then


                    m_Drawing = True
                    RadioButton45.BackColor = Color.Green
                    RadioButton45.ForeColor = Color.White
                    RadioButton45.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)
                        SelectionArea.Stretch(e.X, e.Y)
                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()

                    End Using

                End If

            End If

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PictureBox1_MouseUp(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseUp
        Try

            If e.Button = MouseButtons.Left Then




                If RadioButton2.Checked Then



                    m_Drawing = False

                    RadioButton2.BackColor = Color.White
                    RadioButton2.ForeColor = Color.Black
                    RadioButton2.Invalidate()


                ElseIf RadioButton3.Checked Then

                    m_Drawing = False
                    RadioButton3.BackColor = Color.White
                    RadioButton3.ForeColor = Color.Black
                    RadioButton3.Invalidate()


                ElseIf RadioButton4.Checked Then
                    m_Drawing = True

                    RadioButton4.BackColor = Color.White
                    RadioButton4.ForeColor = Color.Black
                    RadioButton4.Invalidate()

                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawLine(penLine, m_X, m_Y, e.X, e.Y)

                    End Using


                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton5.Checked Then
                    m_Drawing = True
                    RadioButton5.BackColor = Color.White
                    RadioButton5.ForeColor = Color.Black
                    RadioButton5.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawRectangle(penLine, m_X, m_Y,
                          e.X - m_X, e.Y - m_Y)

                    End Using


                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton6.Checked Then
                    m_Drawing = True

                    RadioButton6.BackColor = Color.White
                    RadioButton6.ForeColor = Color.Black
                    RadioButton6.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawEllipse(penLine, m_X, m_Y,
                          e.X - m_X, e.Y - m_Y)

                    End Using


                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton7.Checked Then


                    m_Drawing = False

                    RadioButton7.BackColor = Color.White
                    RadioButton7.ForeColor = Color.Black
                    RadioButton7.Invalidate()


                ElseIf RadioButton8.Checked Then

                    m_Drawing = False

                    RadioButton8.BackColor = Color.White
                    RadioButton8.ForeColor = Color.Black
                    RadioButton8.Invalidate()


                ElseIf RadioButton9.Checked Then
                    m_Drawing = True

                    RadioButton9.BackColor = Color.White
                    RadioButton9.ForeColor = Color.Black
                    RadioButton9.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias



                        Dim Rect = New Rectangle(New Point(m_X, m_Y), New Size(e.X - m_X, e.Y - m_Y))

                        m_Graphics.FillRectangle(Brush1, Rect)

                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()



                ElseIf RadioButton10.Checked Then
                    m_Drawing = True

                    RadioButton10.BackColor = Color.White
                    RadioButton10.ForeColor = Color.Black
                    RadioButton10.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias



                        Dim Rect = New Rectangle(New Point(m_X, m_Y), New Size(e.X - m_X, e.Y - m_Y))

                        m_Graphics.FillEllipse(Brush1, Rect)

                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton11.Checked Then

                    m_Drawing = True

                    RadioButton11.BackColor = Color.White
                    RadioButton11.ForeColor = Color.Black
                    RadioButton11.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                        Dim Rect = New Rectangle(New Point(m_X, m_Y), New Size(e.X - m_X, e.Y - m_Y))

                        Dim GrBrush As LinearGradientBrush

                        GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)


                        m_Graphics.FillRectangle(GrBrush, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton12.Checked Then
                    m_Drawing = True

                    RadioButton12.BackColor = Color.White
                    RadioButton12.ForeColor = Color.Black
                    RadioButton12.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                        Dim Rect = New Rectangle(New Point(m_X, m_Y), New Size(e.X - m_X, e.Y - m_Y))

                        Dim GrBrush As LinearGradientBrush

                        GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)


                        m_Graphics.FillEllipse(GrBrush, Rect)



                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton13.Checked Then

                    m_Drawing = False

                    RadioButton13.BackColor = Color.White
                    RadioButton13.ForeColor = Color.Black
                    RadioButton13.Invalidate()


                ElseIf RadioButton14.Checked Then


                    m_Drawing = False

                    RadioButton14.BackColor = Color.White
                    RadioButton14.ForeColor = Color.Black
                    RadioButton14.Invalidate()



                ElseIf RadioButton44.Checked Then

                    m_Drawing = False

                    RadioButton44.BackColor = Color.White
                    RadioButton44.ForeColor = Color.Black
                    RadioButton44.Invalidate()

                    SelectionArea.Finish()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        Dim Rect = New Rectangle(New Point(SelectionArea.Rectangle.X, SelectionArea.Rectangle.Y), New Size(SelectionArea.Rectangle.Width - m_X, SelectionArea.Rectangle.Height - m_Y))


                        'Dim Rect = New Rectangle(New Point(m_X, m_Y), New Size(e.X - m_X, e.Y - m_Y))

                        m_Graphics.FillEllipse(Brush1, Rect)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton17.Checked Then
                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then
                        m_Drawing = False

                        RadioButton17.BackColor = Color.White
                        RadioButton17.ForeColor = Color.Black
                        RadioButton17.Invalidate()
                    Else
                        Exit Sub

                    End If

                ElseIf RadioButton18.Checked Then

                    m_Drawing = True

                    RadioButton18.BackColor = Color.White
                    RadioButton18.ForeColor = Color.Black
                    RadioButton18.Invalidate()


                    If PointLim < 1 Then Exit Sub


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        ' Dim br As New SolidBrush(Color1)

                        m_Graphics.FillClosedCurve(Brush1, AllPoints)

                        ' Dim demoPen As New Pen(penColor, ToolStripComboBox1.SelectedItem)
                        ' demoPen.Color = penColor
                        m_Graphics.DrawLines(penLine, AllPoints)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton19.Checked Then

                    m_Drawing = True

                    RadioButton19.BackColor = Color.White
                    RadioButton19.ForeColor = Color.Black
                    RadioButton19.Invalidate()


                    If PointLim < 1 Then Exit Sub


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias



                        Dim Rect As New Rectangle(e.X, e.Y, e.X, e.X)

                        Dim GrBrush As LinearGradientBrush

                        GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)



                        m_Graphics.FillClosedCurve(GrBrush, AllPoints)

                        ' Dim demoPen As New Pen(penColor, ToolStripComboBox1.SelectedItem)
                        ' demoPen.Color = penColor

                        m_Graphics.DrawLines(penLine, AllPoints)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton21.Checked Then
                    If TextureBrushPictureToolStripMenuItem.Enabled = True Then


                        m_Drawing = False



                        RadioButton21.BackColor = Color.White
                        RadioButton21.ForeColor = Color.Black
                        RadioButton21.Invalidate()
                    Else
                        Exit Sub
                    End If


                ElseIf RadioButton27.Checked Then

                    m_Drawing = True

                    RadioButton27.BackColor = Color.White
                    RadioButton27.ForeColor = Color.Black
                    RadioButton27.Invalidate()

                    penLine.StartCap() = CType(ComboBox9.SelectedItem, LineCap)

                    penLine.EndCap() = CType(ComboBox10.SelectedItem, LineCap)

                    penLine.DashCap() = CType(ComboBox11.SelectedItem, DashCap)

                    penLine.DashStyle() = CType(ComboBox12.SelectedItem, DashStyle)



                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        m_Graphics.DrawLine(penLine, m_X, m_Y, e.X, e.Y)

                    End Using


                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton29.Checked Then


                    RadioButton29.BackColor = Color.White
                    RadioButton29.ForeColor = Color.Black
                    RadioButton29.Invalidate()

                    Using m_Bitmap = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        If AllPoints1 = 4 Then

                            Dim DrawBZPen As New Pen(CType(ToolStripComboBox2.SelectedItem, Color), CSng(ToolStripComboBox1.SelectedItem))

                            '        canvas.DrawBezier
                            m_Bitmap.DrawBezier(DrawBZPen, startP, PointC1, PointC2, EndP)


                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then


                                If AllPoints1 <= 4 Then

                                    For Each scanPoint In m_Points



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                Else
                                    AllPoints1 += 1

                                End If

                            End If


                        End If



                    End Using

                    m_Points.ToList.Clear()

                    PictureBox1.Refresh()


                ElseIf RadioButton30.Checked Then

                    m_Drawing = True

                    RadioButton30.BackColor = Color.White
                    RadioButton30.ForeColor = Color.Black
                    RadioButton30.Invalidate()


                    If AllPoints2 = 7 Then



                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                            {
                                  _MD_1,
                                  _MD_2,
                                  _MD_3,
                                  _MD_4,
                                  _MD_5,
                                  _MD_6,
                                  _MD_7
                                          }


                            'Dim PenCurve1 As New Pen(CType(ToolStripComboBox2.SelectedItem, Color), ToolStripComboBox1.SelectedItem)


                            'Dim PenCurve2 As New Pen(CType(ToolStripComboBox4.SelectedItem, Color), ToolStripComboBox3.SelectedItem)

                            ' draw curves to screen.

                            m_Graphics.DrawLines(penLine, curvePoints)


                            'm_Graphics.DrawCurve(PenCurve2, curvePoints)



                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 7 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                            scanPoint.X - PointSize,
                            scanPoint.Y - PointSize,
                            PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points2.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points2.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()



                        PictureBox1.Refresh()


                    End If

                ElseIf RadioButton31.Checked Then

                    m_Drawing = True

                    RadioButton31.BackColor = Color.White
                    RadioButton31.ForeColor = Color.Black
                    RadioButton31.Invalidate()


                    If AllPoints2 = 7 Then



                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                            {
                                  _MD_1,
                                  _MD_2,
                                  _MD_3,
                                  _MD_4,
                                  _MD_5,
                                  _MD_6,
                                  _MD_7
                                          }


                            ' Create tension and fill mode.
                            Dim tension As Single = NumericUpDown1.Value
                            Dim aFillMode As FillMode = FillMode.Alternate

                            m_Graphics.DrawClosedCurve(penLine, curvePoints, tension, aFillMode)






                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 7 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                            scanPoint.X - PointSize,
                            scanPoint.Y - PointSize,
                            PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points2.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points2.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()



                        PictureBox1.Refresh()


                    End If
                ElseIf RadioButton32.Checked = True Then


                    m_Drawing = True

                    RadioButton32.BackColor = Color.White
                    RadioButton32.ForeColor = Color.Black
                    RadioButton32.Invalidate()


                    If AllPoints2 = 4 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                                {
                                      MD_1,
                                      MD_2,
                                      MD_3,
                                      MD_4
                                             }



                            ' Create tension and fill mode.
                            Dim tension As Single = NumericUpDown1.Value
                            Dim aFillMode As FillMode = FillMode.Alternate

                            m_Graphics.FillClosedCurve(Brush1, curvePoints)







                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 4 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()

                        PictureBox1.Refresh()

                    End If


                ElseIf RadioButton33.Checked = True Then

                    m_Drawing = True

                    RadioButton33.BackColor = Color.White
                    RadioButton33.ForeColor = Color.Black
                    RadioButton33.Invalidate()

                    If AllPoints2 = 4 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                                {
                                      MD_1,
                                      MD_2,
                                      MD_3,
                                      MD_4
                                             }

                            Dim GBrush As New LinearGradientBrush(MD_1, MD_3, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color))




                            m_Graphics.FillClosedCurve(GBrush, curvePoints)







                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 4 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()

                        PictureBox1.Refresh()

                    End If


                ElseIf RadioButton34.Checked = True Then


                    m_Drawing = True

                    RadioButton34.BackColor = Color.White
                    RadioButton34.ForeColor = Color.Black
                    RadioButton34.Invalidate()

                    If AllPoints2 = 7 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                                {
                                      _MD_1_,
                                      _MD_2_,
                                      _MD_3_,
                                      _MD_4_,
                                      _MD_5_,
                                      _MD_6_,
                                      _MD_7_
                                              }


                            'Dim PenCurve1 As New Pen(CType(ToolStripComboBox2.SelectedItem, Color), ToolStripComboBox1.SelectedItem)



                            m_Graphics.DrawPolygon(penLine, curvePoints)




                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 7 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()

                        PictureBox1.Refresh()

                    End If


                ElseIf RadioButton35.Checked = True Then

                    m_Drawing = True

                    RadioButton35.BackColor = Color.White
                    RadioButton35.ForeColor = Color.Black
                    RadioButton35.Invalidate()

                    If AllPoints2 = 7 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                                {
                                      _MD_1_,
                                      _MD_2_,
                                      _MD_3_,
                                      _MD_4_,
                                      _MD_5_,
                                      _MD_6_,
                                      _MD_7_
                                              }




                            m_Graphics.FillPolygon(Brush1, curvePoints)




                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 7 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()

                        PictureBox1.Refresh()

                    End If

                ElseIf RadioButton36.Checked = True Then

                    m_Drawing = True

                    RadioButton36.BackColor = Color.White
                    RadioButton36.ForeColor = Color.Black
                    RadioButton36.Invalidate()

                    If AllPoints2 = 7 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim curvePoints As Point() =
                                {
                                      _MD_1_,
                                      _MD_2_,
                                      _MD_3_,
                                      _MD_4_,
                                      _MD_5_,
                                      _MD_6_,
                                      _MD_7_
                                              }


                            Dim GBrush As New LinearGradientBrush(_MD_1_, _MD_5_, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color))



                            m_Graphics.FillPolygon(GBrush, curvePoints)




                            If PointsControlToolStripMenuItem.BackColor = Color.Green Then

                                If AllPoints2 <= 7 Then

                                    For Each scanPoint In m_Points2



                                        m_Graphics.FillEllipse(Brushes.Black,
                                scanPoint.X - PointSize,
                                scanPoint.Y - PointSize,
                                PointSize * 2, PointSize * 2)

                                    Next scanPoint
                                    m_Points.ToList.Clear()
                                    scanPoint.ToString.ToList.Clear()

                                Else
                                    AllPoints2 += 1

                                End If


                            End If



                        End Using

                        m_Points.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()

                        PictureBox1.Refresh()

                    End If


                ElseIf RadioButton38.Checked = True Then

                    m_Drawing = True

                    RadioButton38.BackColor = Color.White
                    RadioButton38.ForeColor = Color.Black
                    RadioButton38.Invalidate()

                    If AllPoints2 >= 3 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim Points As Point() =
                                {
                                      MD1,
                                      MD2,
                                      MD3
                                   }




                            Dim path As New GraphicsPath()
                            path.AddLines(Points)


                            Dim pthGrBrush As New PathGradientBrush(path) With {
                            .CenterColor = CType(ComboBox13.SelectedItem, Color)
                        }

                            Dim colors As Color() = {
                         CType(ComboBox14.SelectedItem, Color),
                         CType(ComboBox15.SelectedItem, Color)}



                            pthGrBrush.SurroundColors = colors



                            m_Graphics.FillPath(pthGrBrush, path)


                        End Using

                        m_Points1.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()



                        PictureBox1.Refresh()



                    End If

                ElseIf RadioButton39.Checked = True Then

                    m_Drawing = True

                    RadioButton39.BackColor = Color.White
                    RadioButton39.ForeColor = Color.Black
                    RadioButton39.Invalidate()


                    If AllPoints2 >= 7 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim Points As Point() =
                                {
                                      _MD_1_,
                                      _MD_2_,
                                      _MD_3_,
                                      _MD_4_,
                                      _MD_5_,
                                      _MD_6_,
                                      _MD_7_
                                              }




                            Dim path As New GraphicsPath()
                            path.AddLines(Points)


                            Dim pthGrBrush As New PathGradientBrush(path) With {
                            .CenterColor = CType(ComboBox13.SelectedItem, Color)
                        }

                            Dim colors As Color() = {
                         CType(ComboBox14.SelectedItem, Color),
                         CType(ComboBox15.SelectedItem, Color),
                         CType(ComboBox16.SelectedItem, Color),
                         CType(ComboBox17.SelectedItem, Color),
                        CType(ComboBox18.SelectedItem, Color),
                         CType(ComboBox19.SelectedItem, Color)
                       }

                            pthGrBrush.SurroundColors = colors



                            m_Graphics.FillPath(pthGrBrush, path)







                        End Using

                        m_Points1.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()



                        PictureBox1.Refresh()

                    End If

                ElseIf RadioButton40.Checked = True Then

                    m_Drawing = True

                    RadioButton40.BackColor = Color.White
                    RadioButton40.ForeColor = Color.Black
                    RadioButton40.Invalidate()


                    If AllPoints2 >= 10 Then


                        Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                            m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                            Dim Points As Point() =
                                {
                                        MD_1_,
                                        MD_2_,
                                        MD_3_,
                                        MD_4_,
                                        MD_5_,
                                        MD_6_,
                                        MD_7_,
                                        MD_8_,
                                        MD_9_,
                                        MD_10_
                                              }




                            Dim path As New GraphicsPath()
                            path.AddLines(Points)


                            Dim pthGrBrush As New PathGradientBrush(path) With {
                            .CenterColor = CType(ComboBox13.SelectedItem, Color)
                        }

                            Dim colors As Color() = {
                         CType(ComboBox14.SelectedItem, Color),
                         CType(ComboBox15.SelectedItem, Color),
                         CType(ComboBox16.SelectedItem, Color),
                         CType(ComboBox17.SelectedItem, Color),
                         CType(ComboBox18.SelectedItem, Color),
                         CType(ComboBox19.SelectedItem, Color),
                         CType(ComboBox20.SelectedItem, Color),
                         CType(ComboBox21.SelectedItem, Color),
                         CType(ToolStripComboBox2.SelectedItem, Color)
                       }

                            pthGrBrush.SurroundColors = colors



                            m_Graphics.FillPath(pthGrBrush, path)



                        End Using

                        m_Points1.ToList.Clear()

                        scanPoint.ToString.ToList.Clear()


                        PictureBox1.Refresh()


                    End If
                ElseIf RadioButton41.Checked Then

                    m_Drawing = False

                    RadioButton41.BackColor = Color.White
                    RadioButton41.ForeColor = Color.Black
                    RadioButton41.Invalidate()

                    SelectionArea.Finish()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        Dim Rect = New Rectangle(New Point(SelectionArea.Rectangle.X, SelectionArea.Rectangle.Y), New Size(SelectionArea.Rectangle.Width - m_X, SelectionArea.Rectangle.Height - m_Y))

                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()



                        m_Graphics.DrawRectangle(penLine, Rect)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton42.Checked Then

                    m_Drawing = False

                    RadioButton42.BackColor = Color.White
                    RadioButton42.ForeColor = Color.Black
                    RadioButton42.Invalidate()

                    SelectionArea.Finish()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        Dim Rect = New Rectangle(New Point(SelectionArea.Rectangle.X, SelectionArea.Rectangle.Y), New Size(SelectionArea.Rectangle.Width - m_X, SelectionArea.Rectangle.Height - m_Y))

                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()



                        m_Graphics.DrawEllipse(penLine, Rect)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton43.Checked Then

                    m_Drawing = False

                    RadioButton43.BackColor = Color.White
                    RadioButton43.ForeColor = Color.Black
                    RadioButton43.Invalidate()

                    SelectionArea.Finish()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        Dim Rect = New Rectangle(New Point(SelectionArea.Rectangle.X, SelectionArea.Rectangle.Y), New Size(SelectionArea.Rectangle.Width - m_X, SelectionArea.Rectangle.Height - m_Y))


                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()


                        m_Graphics.FillRectangle(Brush1, Rect)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()


                ElseIf RadioButton44.Checked Then

                    m_Drawing = False

                    RadioButton44.BackColor = Color.White
                    RadioButton44.ForeColor = Color.Black
                    RadioButton44.Invalidate()

                    SelectionArea.Finish()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)


                        m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                        Dim Rect = New Rectangle(New Point(SelectionArea.Rectangle.X, SelectionArea.Rectangle.Y), New Size(SelectionArea.Rectangle.Width - m_X, SelectionArea.Rectangle.Height - m_Y))

                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()



                        m_Graphics.FillEllipse(Brush1, Rect)


                    End Using

                    ' Display the result.
                    PictureBox1.Refresh()

                ElseIf RadioButton45.Checked Then
                    ' for digital number of meanings only

                    m_Drawing = False

                    RadioButton45.BackColor = Color.White
                    RadioButton45.ForeColor = Color.Black
                    RadioButton45.Invalidate()


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)
                        SelectionArea.Finish()
                        ToolStripTextBox4.Text() = SelectionArea.Rectangle.ToString()




                    End Using

                End If

                m_Graphics.Dispose()


            End If



            Clipboard.SetImage(PictureBox1.Image)
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PictureBox1_MouseDown7337(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        Try
            m_Drawing = True

            If e.Button = MouseButtons.Left Then



                If RadioButton46.Checked Then


                    Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                        m_Graphics.CopyFromScreen(New Point(CInt(NumericUpDown8.Value), CInt(NumericUpDown9.Value)), New Point(e.X, e.Y), New Size(CInt(NumericUpDown10.Value), CInt(NumericUpDown11.Value)), CopyPixelOperation.MergePaint)


                        'CopyFromScreen(Me.Location, New Point(40, 70), New Size(500, 500))

                        '
                    End Using

                    PictureBox1.Refresh()
                End If


            End If


        Catch ex As Exception

        End Try

    End Sub



    Private Sub RadioButton23_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton23.CheckedChanged
        If RadioButton23.Checked Then

            RadioButton23.BackColor = Color.DarkGreen
        Else
            RadioButton23.BackColor = Color.White


        End If
    End Sub
    Private Sub PictureBox1_MouseDoubleClick1(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDoubleClick


        Try

            If Not (RadioButton23.BackColor = Color.DarkGreen) Then Exit Sub


            m_Drawing = True

            If e.Button = MouseButtons.Left Then


                Dim drawFormat As New StringFormat()


                Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                    m_Graphics.SmoothingMode = SmoothingMode.AntiAlias

                    Dim drawFont As New Font(CType(ComboBox1.SelectedItem, String), (CType(ComboBox2.SelectedItem, Integer)))

                    Dim drawBrush As New SolidBrush(CType(ComboBox3.SelectedItem, Color))

                    m_Graphics.DrawString(TextBox1.Text(), drawFont, drawBrush,
                    e.X, e.Y, drawFormat)


                    PictureBox1.Refresh()



                End Using
            End If

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try



    End Sub




    Private Sub PictureBox1_DoubleClick(sender As Object, e As EventArgs) Handles PictureBox1.DoubleClick
        Try
            SendKeys.Send("%{PRTSC}")

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub PictureBox1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDoubleClick

        Try
                    If e.Button = MouseButtons.Right Then
                        If (SaveFileDialog1.ShowDialog() = DialogResult.Cancel) Then



                            ToolStrip1.Visible = True
                            MenuStrip1.Visible = True
                            TabControl1.Visible = True
                            FormBorderStyle = FormBorderStyle.Fixed3D

                            SaveFileDialog1.Dispose()


                        Else

                            Dim ContrastBmp = New Bitmap(PictureBox1.Image)


                                If (SaveFileDialog1.FileName.Contains(".bmp")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Bmp)

                                If (SaveFileDialog1.FileName.Contains(".jpeg")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Jpeg)

                                If (SaveFileDialog1.FileName.Contains(".gif")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Gif)

                                If (SaveFileDialog1.FileName.Contains(".png")) Then ContrastBmp.Save(SaveFileDialog1.FileName, ImageFormat.Png)

                            MessageBox.Show("The result image saved under " + SaveFileDialog1.FileName)

                            ContrastBmp.Dispose()


                                ToolStrip1.Visible = True
                                MenuStrip1.Visible = True
                                TabControl1.Visible = True
                                FormBorderStyle = FormBorderStyle.Fixed3D

                            End If

                        End If



                Catch ex As Exception
            If CBool(MessageBox.Show("The file " + SaveFileDialog1.FileName + " has an inappropriate extension. Returning.")) Then
                Return
            End If

        End Try



    End Sub

    Private Sub GrBackColorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GrBackColorToolStripMenuItem.Click
        Try

            Using m_Graphics = Graphics.FromImage(PictureBox1.Image)
                m_Graphics.SmoothingMode = SmoothingMode.AntiAlias


                Dim GrBrush As LinearGradientBrush


                Dim Rect = New Rectangle(0, 0, PictureBox1.ClientSize.Width, PictureBox1.ClientSize.Height)


                GrBrush = New LinearGradientBrush(Rect, CType(ToolStripComboBox2.SelectedItem, Color), CType(ToolStripComboBox3.SelectedItem, Color), NumericUpDown4.Value)



                m_Graphics.FillRectangle(GrBrush, Rect)

                PictureBox1.Invalidate()

            End Using

        Catch ex As Exception
            MessageBox.Show(ex.Message, "Mistake", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

        End Try

    End Sub

    Private Sub RadioButton17_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton17.CheckedChanged
        Try
            TextureBrushPictureToolStripMenuItem.Enabled = True


        Catch ex As Exception

        End Try
    End Sub

    Private Sub RadioButton20_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton20.CheckedChanged
        Try
            If RadioButton17.Checked = False Then

                TextureBrushPictureToolStripMenuItem.Enabled = False

            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub RadioButton21_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton21.CheckedChanged
        Try
            TextureBrushPictureToolStripMenuItem.Enabled = True

        Catch ex As Exception

        End Try

    End Sub

    Private Sub SystemOfCoordinateToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SystemOfCoordinateToolStripMenuItem.Click
        Try

            If RadioButton25.Checked Then

                Dim LineCap As New LineCap
                'Создание объекта рисования

                'ОБъявление и декларация переменных для задания параметров системы координат
                Dim a As Integer = CInt(ComboBox4.SelectedItem)
                Dim b As Integer = CInt(ComboBox5.SelectedItem)
                Dim c As Integer = CInt(ComboBox6.SelectedItem)
                Dim d As Integer = CInt(ComboBox7.SelectedItem)
                Dim f As Integer = CInt(ComboBox8.SelectedItem)

                Using m_Graphics = Graphics.FromImage(PictureBox1.Image)

                    ' Перо для осей
                    Dim pnXY As New Pen(Color.Black, f)

                    pnXY.EndCap() = LineCap.ArrowAnchor



                    ' Ширина и высота клетки
                    Dim width As Integer = a, height As Integer = b

                    ' Число клеток по горизонтали и вертикали
                    Dim countX As Integer = c, countY As Integer = d
                    ' Рисуем линии


                    For i As Integer = 1 To countX
                        For j As Integer = 1 To countY

                            m_Graphics.DrawLine(Pens.Black, width, height * i, width * countY, height * i)
                            m_Graphics.DrawLine(Pens.Black, width * j, height, width * j, height * countX)
                            ' Рисуем оси

                            If i = Math.Ceiling(countX / 2) Then
                                m_Graphics.DrawLine(pnXY, width, height * i, width * (countY + 1), height * i)
                            End If

                            If j = Math.Ceiling(countY / 2) Then
                                m_Graphics.DrawLine(pnXY, width * j, height * countX, width * j, 0)
                            End If


                        Next j

                    Next i

                End Using

                PictureBox1.Refresh()




            End If



        Catch ex As Exception

        End Try

    End Sub


#End Region




End Class
