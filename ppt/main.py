from manim import *
from manim_slides import *

class MyPpt(Slide):
    def construct(self):
        line_top = Line(LEFT*6 + UP*2, RIGHT*6 + UP*2, color=RED)
        line_bottom = Line(LEFT*6 + DOWN*2, RIGHT*6 + DOWN*2, color=RED)
        self.add(line_top, line_bottom) # 2+2=4 이므로 이 사이에 딱 들어와야 함
        self.wait()
        self.next_slide()

    def show_intro(self):
        title = Text("제임스와 존의 곱셈의 비밀").scale(0.5)
        subtitle = Text(": 수비학적 변환").scale(0.3)
        subtitle.next_to(title, DOWN)
        titles = VGroup(title, subtitle)

        self.play(FadeIn(titles))
        self.next_slide()
        self.play(FadeOut(titles))
        self.next_slide()

    def show_index(self):
        mokcha_list = [ "곱셈이란?", "빠른 곱셈의 비결", "시계나라의 숫자들", "시연"]
        mokcha = VGroup()
        for i, text in enumerate(mokcha_list):
            dot = Dot().scale(0.4)
            mtext = Text(text).scale(0.4)
            tmp = VGroup(dot, mtext).arrange(RIGHT, buff=0.4)
            mokcha.add(tmp)
        mokcha.arrange(DOWN, aligned_edge=LEFT, buff=0.4).center()

        for i in mokcha:
            self.play(FadeIn(i, shift=RIGHT*0.5))
            self.next_slide()
        self.play(FadeOut(mokcha))

    def show_gopsem(self):
        imgs_names = ['1x1', '2x2', '3x2']
        imgs = Group()
        for i in imgs_names:
            img = ImageMobject(f"./assets/{i}.jpg")
            img.height = 4
            imgs.add(img)
        imgs.arrange(RIGHT, buff=0.5)
        self.play(FadeIn(imgs.center()))
        self.next_slide()
