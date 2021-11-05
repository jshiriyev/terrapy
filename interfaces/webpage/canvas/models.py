from django.db import models

# Create your models here.

class Course(models.Model):
    title = models.CharField(max_length=150)
    intro = models.TextField()
    logopath = models.TextField()
    #logopath = forms.TextField(default="",widget=Textarea(attrs={"rows":1}))

    def __str__(self):
        return self.title

#class CourseModelForm(ModelForm):
#    class Meta:
#        model = Course
#        widgets = {
#                'logopath': Textarea(attrs={'rows':1}),
#                }
