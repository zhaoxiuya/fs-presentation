from manim import *
from manim_slides import *
import numpy as np
import math

class MyPpt(Slide, MovingCameraScene):
    BZ = 1.5
    MZ = 1.0
    SZ = 0.5

    def construct(self):
        self.show_intro()
        self.show_index()
        self.show_gopsem()
        self.show_multiply()
        self.show_poly()
        self.show_numol()
        self.show_poly_n()
        self.show_complex()
        self.show_fft()

    def show_intro(self):
        title = Text("제임스와 존의 곱셈의 비밀").scale(self.BZ)
        subtitle = Text("부제: 수비학적 변환").scale(self.MZ)
        subtitle.next_to(title, DOWN)
        titles = VGroup(title, subtitle)

        self.play(FadeIn(titles.center()))
        self.next_slide()
        self.play(FadeOut(titles))
        self.next_slide()

    def show_index(self):
        mokcha_list = [ "곱셈이란?", "시계나라의 숫자들", "빠른 곱셈의 비결", "시연"]
        mokcha = VGroup()
        for i, text in enumerate(mokcha_list):
            dot = Dot().scale(self.MZ)
            mtext = Text(text).scale(self.MZ)
            tmp = VGroup(dot, mtext).arrange(RIGHT)
            mokcha.add(tmp)
        mokcha.arrange(DOWN, aligned_edge=LEFT, buff=1.0).center()

        for i in mokcha:
            self.play(FadeIn(i, shift=RIGHT*0.5))
            self.next_slide()
        self.play(FadeOut(mokcha))

    def show_gopsem(self):
        imgs_names = ['1x1', '2x2', '3x2']
        imgs = Group()
        for i in imgs_names:
            img = ImageMobject(f"./assets/{i}.jpg")
            img.height = 5
            text = Text(i)
            tmp = Group(img, text).arrange(DOWN).center()
            imgs.add(tmp)
        imgs.arrange(RIGHT, buff=1.0)
        for i in imgs:
            self.play(FadeIn(i, shift=RIGHT*0.5))
            self.next_slide()
        self.play(FadeOut(imgs))
        self.next_slide()

    def show_multiply(self):
        a_str, b_str = "260131", "091227"

        a = VGroup(*[MathTex(i) for i in a_str]).arrange(RIGHT, buff=0.2)
        b = VGroup(*[MathTex(i) for i in b_str]).arrange(RIGHT, buff=0.2)
        b.next_to(a, DOWN, aligned_edge=RIGHT)

        x = MathTex(r"\times").next_to(b, LEFT).scale(self.MZ)
        line = Line(LEFT, RIGHT).set_width(a.width + 0.8).next_to(b, DOWN, buff=0.1)

        setup = VGroup(a, b, x, line).to_edge(UP, buff=0.5)
        self.play(Write(setup))
        self.next_slide()

        l = [VGroup() for i in range(len(b_str))]
        for i in range(len(b_str))[::-1]:
            tmp = str(int(b_str[i])*int(a_str))
            if len(tmp) < len(b_str):
                tmp = "0"*(len(b_str)-len(tmp)) + tmp
            l[i] = VGroup(*[MathTex(x) for x in tmp]).arrange(RIGHT, buff=0.2)
        l += [b]

        for i in range(len(b_str))[::-1]:
            l[i].next_to(l[i+1], DOWN, aligned_edge=RIGHT)
            self.play(b[i].animate.set_color(RED))
            if i!=len(b_str)-1:
                l[i].shift(LEFT * 0.4)
            for j in range(len(b_str))[::-1]:
                self.play(a[j].animate.set_color(BLUE),run_time=0.15)
                if(len(b_str)<len(l[i])):
                    if j == 0:
                        self.play(Write(l[i][j+1].set_color(GOLD)),run_time=0.15)
                        l[i][j+1].set_color(WHITE)
                        self.play(Write(l[i][j].set_color(GOLD)),run_time=0.15)
                        l[i][j].set_color(WHITE)
                    else:
                        self.play(Write(l[i][j+1].set_color(GOLD)),run_time=0.15)
                        l[i][j+1].set_color(WHITE)
                else:
                    self.play(Write(l[i][j].set_color(GOLD)),run_time=0.15)
                    l[i][j].set_color(WHITE)
                self.play(a[j].animate.set_color(WHITE),run_time=0.15)
            self.play(b[i].animate.set_color(WHITE),run_time=0.15)

        self.next_slide()

        plus = MathTex(r"+").scale(self.MZ)
        plus.next_to(l[0][0], LEFT, buff=0.3)

        line2 = Line(LEFT, RIGHT)
        line2.set_width(line.width * 2)
        line2.next_to(l[0], DOWN, buff=0.1)
        line2.align_to(line, RIGHT)

        self.play(Write(VGroup(plus, line2)))
        self.next_slide()

        c_str = str(int(a_str) * int(b_str))
        c = VGroup(*[MathTex(i) for i in c_str]).arrange(RIGHT, buff=0.2)
        c.next_to(line2, DOWN, buff=0.25)
        c.align_to(b, RIGHT)

        for i in range(len(c_str))[::-1]:
            for j in range(len(b_str)):
                if(len(a_str)<len(l[j])):
                    for k in range(len(a_str)):
                        if j + k == i:
                            self.play(l[j][k+1].animate.set_color(RED), run_time=0.1)
                        if j + k-1 == i and k==0:
                                self.play(l[j][k].animate.set_color(RED), run_time=0.1)
                else:
                    for k in range(len(a_str)):
                        if j + k == i:
                            self.play(l[j][k].animate.set_color(RED), run_time=0.1)

            self.play(Write(c[i].set_color(GOLD)),run_time=0.15)
            c[i].set_color(WHITE)

            for j in range(len(b_str)):
                if(len(a_str)<len(l[j])):
                    for k in range(len(a_str)):
                        if j + k == i:
                            self.play(l[j][k+1].animate.set_color(WHITE), run_time=0.1)
                        if j + k-1 == i and k==0:
                           self.play(l[j][k].animate.set_color(WHITE), run_time=0.1)
                else:
                    for k in range(len(a_str)):
                        if j + k == i:
                            self.play(l[j][k].animate.set_color(WHITE), run_time=0.1)

        self.next_slide()

        self.play(FadeOut(line, line2, plus, x, a, b))

        self.play(FadeOut(c))

        for i in l:
            for j in i:
                self.play(FadeOut(j))

        self.next_slide()

    def show_poly(self):
        expr = MathTex("(x^2 + 3x + 2)(2x + 3)", substrings_to_isolate=["x^2", "3x", " 2", "2x", "3"]).center()
        expr.set_color_by_tex("2x", RED_C)
        expr.set_color_by_tex(" 2", BLUE_C)
        expr.set_color_by_tex("3", RED_C)
        expr.set_color_by_tex("3x", BLUE_C)
        expr.set_color_by_tex("x^2", BLUE_C)
        expr.set_color_by_tex("2x", RED_C)

        expr.to_edge(UP)
        self.play(Write(expr))
        self.next_slide()

        self.play(expr.animate.to_edge(UP))
        self.next_slide()

        row = ["x^2", "3x", "2"]
        col = ["2x", "3"]

        table = [
            [("2x^3"), ("3x^2")],
            [("6x^2"), ("9x")],
            [("4x"), ("6")],
        ]

        table = Table(
            table,
            row_labels=[MathTex(t) for t in row],
            col_labels=[MathTex(t) for t in col],
            top_left_entry=MathTex(r"\times"),
            include_outer_lines=True,
            element_to_mobject=MathTex,
            h_buff=1.6,
            v_buff=1.2,
        )

        row_colors = [BLUE_C, BLUE_C, BLUE_C]
        col_colors = [RED_C, RED_C]

        for i, color in enumerate(row_colors):
            table.get_row_labels()[i].set_color(color)

        for j, color in enumerate(col_colors):
            table.get_col_labels()[j].set_color(color)

        table.scale(0.8)
        table.next_to(expr, DOWN, buff=0.8)

        self.play(FadeIn(table))
        self.next_slide()

        self.play(table.animate.shift(LEFT*3.5))

        expr2 = MathTex("= 2x^3 + 9x^2 + 13x + 6")
        expr2.next_to(table, buff=0.8)
        self.play(Write(expr2))

        self.next_slide()

        self.play(FadeOut(expr2, expr, table))

        expr = VGroup(expr, expr2).arrange(RIGHT, buff=0.3)

        self.play(Write(expr))

        self.next_slide()

        self.play(FadeOut(expr))

    def show_numol(self):
        axpr = MathTex("(x^2 + 3x + 2)(2x + 3)", substrings_to_isolate=["x^2", "3x", " 2", "2x", "3"]).center()
        axpr.set_color_by_tex("2x", RED_C)
        axpr.set_color_by_tex(" 2", BLUE_C)
        axpr.set_color_by_tex("3", RED_C)
        axpr.set_color_by_tex("3x", BLUE_C)
        axpr.set_color_by_tex("x^2", BLUE_C)
        axpr.set_color_by_tex("2x", RED_C)
        axpr.to_corner(UL)
        axpr = VGroup(axpr, MathTex("= 2x^3 + 9x^2 + 13x + 6")).arrange(RIGHT, buff=0.3)

        expr = MathTex(r"132 \times 23", substrings_to_isolate=["132", "23"]).center()
        expr.set_color_by_tex("132", BLUE_C)
        expr.set_color_by_tex("23", RED_C)

        expr.to_edge(UP)
        self.play(Write(expr))
        self.next_slide()

        self.play(expr.animate.to_edge(UP))
        self.next_slide()

        row = ["1", "3", "2"]
        col = ["2", "3"]

        table = [
            [("2"), ("3")],
            [("6"), ("9")],
            [("4"), ("6")],
        ]

        table = Table(
            table,
            row_labels=[MathTex(t) for t in row],
            col_labels=[MathTex(t) for t in col],
            top_left_entry=MathTex(r"\times"),
            include_outer_lines=True,
            element_to_mobject=MathTex,
            h_buff=1.6,
            v_buff=1.2,
        )

        row_colors = [BLUE_C, BLUE_C, BLUE_C]
        col_colors = [RED_C, RED_C]

        for i, color in enumerate(row_colors):
            table.get_row_labels()[i].set_color(color)

        for j, color in enumerate(col_colors):
            table.get_col_labels()[j].set_color(color)

        table.scale(0.8)
        table.next_to(expr, DOWN, buff=0.8)

        self.play(FadeIn(table))
        self.next_slide()

        self.play(table.animate.shift(LEFT*3.5))

        expr2 = MathTex(r"= 2 \times 1000 + 9 \times 100 + 13 \times 10 + 6")
        expr2.next_to(table, buff=0.8)
        self.play(Write(expr2))
        self.next_slide()
        adad = MathTex(r" = 3036").center()
        adad.next_to(table, buff=0.8)
        self.play(Transform(expr2, adad))

        self.next_slide()

        self.play(FadeOut(expr2, expr, table))

        expr = VGroup(expr, expr2).arrange(RIGHT, buff=0.3)

        self.play(Write(expr))

        self.next_slide()

        self.play(expr.animate.to_corner(DR))
        adad = MathTex(r"132 \times 23 = 3036", substrings_to_isolate=["132", "23"]).center()
        adad.set_color_by_tex("132", BLUE_C)
        adad.set_color_by_tex("23", RED_C)
        adad.to_corner(DR)
        self.play(Transform(expr, adad))

        self.next_slide()

        self.play(Write(axpr))

        self.play(axpr.animate.to_corner(UL))

        self.next_slide()

        app = MathTex(r'\approx').scale(3.0).center()
        self.play(Write(app))

        self.next_slide()

        self.play(FadeOut(app, expr, axpr))

        self.next_slide()



        self.next_slide()

    def show_poly_n(self):
        f = MathTex(r"f(x) = x^2 + 3x + 2").set_color(BLUE_C)
        f.to_edge(UR)
        g = MathTex(r"g(x) = 2x + 3").set_color(RED_C)
        g.to_edge(DR)
        self.play(Write(f), Write(g))
        self.next_slide()

        axes = Axes(
            x_range=[-5, 5, 1],
            y_range=[-5, 5, 1],
            x_length=10,
            tips=True,
        )
        axes.to_corner(DOWN)
        self.next_slide()

        fx = lambda x: x**2+3*x+2
        gx = lambda x: 2*x+ 3
        fg = lambda x: (x**2+3*x+2) * (2*x+ 3)

        fgraph = axes.plot(fx, color=BLUE, x_range=[-5, 5]).set_color(BLUE_C)
        ggraph = axes.plot(gx, color=BLUE, x_range=[-5, 5]).set_color(RED_C)
        fggraph = axes.plot(fg, color=BLUE, x_range=[-5, 5]).set_color(WHITE)

        self.play(Create(fgraph), run_time=1)
        self.next_slide()
        self.play(Create(ggraph), run_time=1)
        self.next_slide()
        self.play(Create(fggraph), run_time=1)
        self.next_slide()

        self.play(FadeOut(fgraph, ggraph, fggraph))

        aval = -1
        bval = -2
        cval = -3
        dval = 0

        ad = Dot(axes.c2p(aval, gx(aval)), color=WHITE,  radius=0.12)
        bd = Dot(axes.c2p(bval, gx(bval)), color=WHITE,  radius=0.12)
        self.play(Create(ad), Create(bd))

        self.next_slide()
        self.play(Write(ggraph))

        self.play(FadeOut(ad, ggraph, bd))
        self.next_slide()

        ad = Dot(axes.c2p(aval, fx(aval)), color=WHITE,  radius=0.12)
        bd = Dot(axes.c2p(bval, fx(bval)), color=WHITE,  radius=0.12)
        cd = Dot(axes.c2p(cval, fx(cval)), color=WHITE,  radius=0.12)
        self.play(Create(ad), Create(bd), Create(cd))

        self.next_slide()
        self.play(Write(fgraph))

        self.play(FadeOut(ad, fgraph, bd, cd))
        self.next_slide()

        self.play(FadeOut(ad, fgraph, bd, cd))

        self.next_slide()

        self.play(FadeOut(f, g))

        alg = VGroup(axes, fgraph, ggraph, fggraph)
        alg.shift(LEFT*3)

        self.play(Write(fgraph), Write(ggraph), Write(fggraph))

        h = MathTex(r"h(x) = 2x^3 + 9x^2 + 13x + 6")
        h.to_edge(DR)

        self.play(Write(h))

        self.next_slide()

        ad = Dot(axes.c2p(aval, fx(aval)), color=WHITE,  radius=0.12)
        bd = Dot(axes.c2p(bval, fx(bval)), color=WHITE,  radius=0.12)
        cd = Dot(axes.c2p(cval, fx(cval)), color=WHITE,  radius=0.12)
        dd = Dot(axes.c2p(dval, fx(dval)), color=WHITE,  radius=0.12)
        dots = VGroup(ad, bd, cd, dd)
        self.play(Create(ad), Create(bd), Create(cd), Create(dd))

        dota = dots

        self.next_slide()

        fpair = MathTex("{" + f"({aval}, {fx(aval)}),({bval}, {fx(bval)}),({cval}, {fx(cval)}),({dval}, {fx(dval)})" + "}").set_color(BLUE_C)
        fpair.to_edge(UR)
        self.play(Transform(dots, fpair))

        self.next_slide()

        cross = MathTex(r'\times').scale(3.0)
        cross.next_to(fpair, DOWN)
        self.play(Write(cross))

        self.next_slide()

        ad = Dot(axes.c2p(aval, gx(aval)), color=WHITE,  radius=0.12)
        bd = Dot(axes.c2p(bval, gx(bval)), color=WHITE,  radius=0.12)
        cd = Dot(axes.c2p(cval, gx(cval)), color=WHITE,  radius=0.12)
        dd = Dot(axes.c2p(dval, gx(dval)), color=WHITE,  radius=0.12)
        dots = VGroup(ad, bd, cd, dd)
        self.play(Create(ad), Create(bd), Create(cd), Create(dd))

        self.next_slide()

        gpair = MathTex("{" + f"({aval}, {gx(aval)}),({bval}, {gx(bval)}),({cval}, {gx(cval)}),({dval}, {gx(dval)})" + "}").set_color(RED_C)
        gpair.next_to(cross, DOWN)
        self.play(Transform(dots, gpair))

        self.next_slide()

        junim = VGroup(gpair, fpair, cross)

        self.next_slide()

        fgx = fg
        ad = Dot(axes.c2p(aval, fgx(aval)), color=WHITE,  radius=0.12)
        bd = Dot(axes.c2p(bval, fgx(bval)), color=WHITE,  radius=0.12)
        cd = Dot(axes.c2p(cval, fgx(cval)), color=WHITE,  radius=0.12)
        dd = Dot(axes.c2p(dval, fgx(dval)), color=WHITE,  radius=0.12)
        dot2 = VGroup(ad, bd, cd, dd)
        self.play(Create(ad), Create(bd), Create(cd), Create(dd))

        self.next_slide()

        fgpair = MathTex("{" + f"({aval}, {fgx(aval)}),({bval}, {fgx(bval)}),({cval}, {fgx(cval)}),({dval}, {fgx(dval)})" + "}")
        fgpair.move_to(ORIGIN)

        ajunim = VGroup(dot2, junim)
        self.play(Transform(ajunim, fgpair), )
        self.play(FadeOut(fgraph, fggraph, ggraph, fpair, gpair, h, dots, dota))

        self.next_slide()

        self.play(FadeOut(ajunim))

        self.next_slide()

    def show_complex(self):
        plane = ComplexPlane(
            x_range=[-10, 10, 1],
            y_range=[-10, 10, 1],
        ).add_coordinates()

        self.play(Create(plane))

        self.next_slide()

        self.play(
            self.camera.frame.animate.set(width=6).move_to(Dot(plane.n2p(0+0j))),
            run_time=2
        )

        self.next_slide()

        z =  0+1j
        vec = Arrow(start=plane.n2p(0), end=plane.n2p(z), buff=0, color=YELLOW)
        self.play(GrowArrow(vec))
        self.next_slide()
        for i in range(4):
            nz = z * 1j
            self.play(Rotate(vec, angle=PI/2, about_point=plane.n2p(0)))
            z = nz
            self.next_slide()

        self.next_slide()

        self.play(FadeOut(vec))

        z =  math.sqrt(2)/2+math.sqrt(2)/2*1j
        vec = Arrow(start=plane.n2p(0), end=plane.n2p(z), buff=0, color=YELLOW)
        self.play(GrowArrow(vec))
        self.next_slide()
        for i in range(8):
            nz = z * 1j
            self.play(Rotate(vec, angle=PI/4, about_point=plane.n2p(0)), run_time=1.5)
            z = nz
            self.wait(0.5)
        self.next_slide()
        label = MathTex("w_8").next_to(vec.get_end(), UR).set_color(YELLOW)
        self.play(Write(label))
        self.next_slide()

        self.play(FadeOut(vec,label,plane))
        self.play(
            self.camera.frame.animate.set(width=14).move_to(Dot(plane.n2p(0+0j))),
            run_time=2
        )

    def show_fft(self):
        tmptex = MathTex("f(X) = ")
        self.play(Write(tmptex))

        self.next_slide()

        self.play(FadeOut(tmptex))

        data = [[MathTex(fr"w_8^{(i*j)%8}") for j in range(8)] for i in range(8)]
        mat = MobjectMatrix(data).scale(0.7)
        data = [[MathTex(fr"x_{i}") for j in range(1)] for i in range(8)]
        vec = MobjectMatrix(data).scale(0.7)
        vec.next_to(mat)

        VGroup(mat, vec).center()
        self.play(Write(mat))

        TOP = UP*3

        self.next_slide()

        head = MathTex('W').scale(3.0).set_color(YELLOW)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(head))
        self.play(FadeOut(mat))
        self.play(Write(vec))

        self.next_slide()

        head = MathTex('X').scale(3.0).set_color(YELLOW)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(head))

        self.next_slide()
        self.play(Write(mat))

        for i in range(0, 8, 2):
            for j in range(0, 8):
                mat.get_rows()[j][i].set_color(YELLOW)
            vec.get_rows()[i].set_color(YELLOW)
        self.play(Transform(mat, mat), Transform(vec, vec))

        self.next_slide()

        data = [[MathTex(fr"w_8^{(i*j)%8}") for j in [0,2,4,6,1,3,5,7]] for i in range(8)]
        mat2 = MobjectMatrix(data).scale(0.7)
        data = [[MathTex(fr"x_{i}") for j in range(1)] for i in [0,2,4,6,1,3,5,7]]
        vec2 = MobjectMatrix(data).scale(0.7)
        vec2.next_to(mat2)
        VGroup(mat2, vec2).center()

        self.play(Transform(mat, mat2), Transform(vec, vec2))

        self.next_slide()

        tmp1 = []
        tmp2 = []
        tmp3 = []
        tmp4 = []
        for i in range(4):
            for j in range(4):
                tmp3.append(mat.get_rows()[i+4][j])
                tmp2.append(mat.get_rows()[i][j])#
                tmp1.append(mat.get_rows()[i][j+4])
                tmp4.append(mat.get_rows()[i+4][j+4])
        tmp1 = VGroup(*tmp1)
        tmp2 = VGroup(*tmp2)
        tmp3 = VGroup(*tmp3)
        tmp4 = VGroup(*tmp4)

        hb0 = SurroundingRectangle(tmp3, color=BLUE, buff=0.1)
        hb1 = SurroundingRectangle(tmp2, color=BLUE, buff=0.1)

        self.play(Create(hb0))
        self.play(Create(hb1))

        self.next_slide()

        head = MathTex('W_e').scale(3.0).set_color(BLUE)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(head))
        self.play(FadeOut(hb0))
        self.play(FadeOut(hb1))

        self.next_slide()

        self.play(FadeOut(VGroup(mat, vec)))

        data = [[(MathTex(fr"w_8^{(i)%8}") if i==j else MathTex(0)) for j in range(4)] for i in range(4)]
        m = MobjectMatrix(data).scale(0.7).center()
        head.move_to(TOP)
        self.play(Write(m))

        head = MathTex('M_4').scale(3.0).set_color(RED)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(m, head))
        self.play(Write(VGroup(mat, vec)))

        self.next_slide()

        hb0 = SurroundingRectangle(tmp1, color=RED, buff=0.1)
        self.play(Create(hb0))
        head = MathTex('M_4 W_e').scale(3.0).set_color(RED)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(head))
        self.play(FadeOut(hb0))

        self.next_slide()

        hb0 = SurroundingRectangle(tmp4, color=RED, buff=0.1)
        self.play(Create(hb0))
        head = MathTex('-M_4 W_e').scale(3.0).set_color(RED)
        head.move_to(TOP)
        self.play(Write(head))

        self.next_slide()

        self.play(FadeOut(head))
        self.play(FadeOut(hb0))

        self.next_slide()

        FadeOut(mat)
        hb0 = SurroundingRectangle(vec, color=BLUE, buff=0.1)

        self.next_slide()
        FadeOut(VGroup(mat, vec))

        self.next_slide()

        self.next_slide()
