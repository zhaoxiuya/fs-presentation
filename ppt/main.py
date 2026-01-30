from manim import *

class Fs(Scene):
    def construct(self):
        a = Circle()
        circle = Circle()
        square = Square()

        for i in [circle, square] * 10:
            self.play(Transform(a, i))
