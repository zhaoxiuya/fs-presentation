from manim import *
from manim_slides import *
import numpy as np

class MyPpt(Slide):
    BZ = 1.5
    MZ = 1.0
    SZ = 0.5

    def construct(self):
        # self.show_intro()
        # self.show_index()
        # self.show_gopsem()
        self.show_multiply()

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
        mokcha_list = [ "곱셈이란?", "빠른 곱셈의 비결", "시계나라의 숫자들", "시연"]
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

        a = VGroup(*[Text(i) for i in a_str]).arrange(RIGHT, buff=0.2)
        b = VGroup(*[Text(i) for i in b_str]).arrange(RIGHT, buff=0.2)
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
            l[i] = VGroup(*[Text(x) for x in tmp]).arrange(RIGHT, buff=0.2)
        l += [b]

        for i in range(len(b_str))[::-1]:
            l[i].next_to(l[i+1], DOWN, aligned_edge=RIGHT)
            self.play(b[i].animate.set_color(RED))
            if i!=len(b_str)-1:
                l[i].shift(LEFT * 0.5)
            for j in range(len(b_str))[::-1]:
                self.play(a[j].animate.set_color(BLUE),run_time=0.15)
                if(len(b_str)<len(l[i])):
                    if j == 0:
                        self.play(l[i][j+1].animate.set_color(GOLD),run_time=0.15)
                        self.play(FadeIn(l[i][j+1]),run_time=0.15)
                        self.play(l[i][j+1].animate.set_color(WHITE),run_time=0.15)
                        self.play(l[i][j].animate.set_color(GOLD),run_time=0.15)
                        self.play(FadeIn(l[i][j]),run_time=0.15)
                        self.play(l[i][j].animate.set_color(WHITE),run_time=0.15)
                    else:
                        self.play(l[i][j+1].animate.set_color(GOLD),run_time=0.15)
                        self.play(FadeIn(l[i][j+1]),run_time=0.15)
                        self.play(l[i][j+1].animate.set_color(WHITE),run_time=0.15)
                else:
                    self.play(l[i][j].animate.set_color(GOLD),run_time=0.15)
                    self.play(FadeIn(l[i][j]), run_time=0.15)
                    self.play(l[i][j].animate.set_color(WHITE),run_time=0.15)
                self.play(a[j].animate.set_color(WHITE),run_time=0.15)
            self.play(b[i].animate.set_color(WHITE),run_time=0.15)


        line2 = (Line(LEFT, RIGHT).set_width(2 * a.width + 0.8).next_to(l[0], DOWN, buff=0.1, aligned_edge=RIGHT).align_to(b, RIGHT))
        plus = MathTex(r"+").next_to(b, LEFT).scale(self.MZ)

        setup2 = VGroup(plus, line2).to_edge(UP, buff=0.5)
        self.play(Write(setup2))
        self.next_slide()
