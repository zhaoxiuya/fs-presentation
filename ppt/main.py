from manim import *
from manim_slides import *

class MyPpt(Slide):
    BZ = 1.5
    MZ = 1.0
    SZ = 0.5

    def construct(self):
        self.show_intro()
        self.show_index()
        self.show_gopsem()

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
