"""mysite URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.1/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.urls import path,re_path

from django.views.generic.detail import DetailView
from django.views.generic.list import ListView

from personal.views import home_view
from personal.views import bio_view
from personal.views import contact_view

from blog.models import Post

from canvas.models import Course

urlpatterns = [
    path('admin/',admin.site.urls),
    path('',home_view,name='home'),

    path('blog/',ListView.as_view(queryset=Post.objects.all().order_by("-date")[:25],\
                 template_name="bloglist.html")),
    path('blog/<int:pk>/',DetailView.as_view(model=Post,template_name='blogpost.html')),

    path('courses/',ListView.as_view(queryset=Course.objects.all().order_by("title")[:25],\
                    template_name="courselist.html")),
    path('courses/<int:pk>/',DetailView.as_view(model=Course,template_name="courseinfo.html")),

    path('bio/',bio_view,name="bio"),
    path('contact/',contact_view,name="contact"),
]
