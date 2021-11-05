from django.shortcuts import render

# Create your views here.

def home_view(request,*args,**kwargs):
    return render(request,"home.html",{})

def bio_view(request,*args,**kwargs):
    return render(request,"bio.html",{})

def contact_view(request,*args,**kwargs):
    my_context = {
        "my_text": "Whatsapp Nomremiz",
        "my_email": "(050)316-1227",
    }
    return render(request,"contact.html",my_context)
