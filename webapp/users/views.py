from django.shortcuts import render, redirect
from django.contrib.auth.forms import  AuthenticationForm
from django.contrib.auth import login, logout
from django.contrib.auth.decorators import login_required
from django.contrib import messages
from .forms import UserRegisterForm, UserUpdateForm, ProfileUpdateForm, AddressUpdateForm
# Create your views here.

def singup_view(request):
    if request.method == 'POST':
        #Validate user data
        form = UserRegisterForm(request.POST)
        if form.is_valid():
            user =form.save() #save user data to database, and fetch user name
            messages.success(request, f'Your account has been create! You are now able to log in') #flash message
            login(request,user)
            return redirect('users:login') #a view in an app
    else: # if it is a GET request
        form = UserRegisterForm()
    # return from. If Get request it returns a newly create form, if Post request but form is invalid, it returns a the form filled in by the usr.
    return render(request, 'users/signup-2.html', {'form':form})

def login_view(request):
    if request.method == 'POST':
        form = AuthenticationForm(data=request.POST) #validate username and password
        if form.is_valid():
            user = form.get_user() # fetch user name
            login(request, user) # log in user
            return redirect('hypergrass:missions')
    else:
        form = AuthenticationForm() # create new empty form
    return render(request, 'users/login.html', {'form':form})

def logout_view(request):
    # if request.method=='POST':
    logout(request)
    return render(request, 'users/logout.html')

def index(request):
    return render(request, 'users/home.html')

@login_required
def profile_view(request):
    if request.method == 'POST': # Update profile data in database
        usr_form =UserUpdateForm(request.POST, instance=request.user)
        prof_form = ProfileUpdateForm(request.POST, request.FILES, instance=request.user.profile)
        # ad_form =AddressUpdateForm(request.POST, request.content_params, instance=request.address)
        if usr_form.is_valid() and prof_form.is_valid(): # validate data from all forms
            usr_form.save()
            prof_form.save()
            # ad_form.save()
            messages.success(request, f'Your account has been updated')  # flash message
            return redirect('users:profile') # avoid issues with post-get-redirect pattern

    else: # show current profile data
        usr_form =UserUpdateForm(instance=request.user)
        prof_form = ProfileUpdateForm(instance=request.user.profile)
        # ad_form = AddressUpdateForm(instance=request.address)
# TODO: add functionality to update address
    context ={
        'u_form': usr_form,
        'p_form': prof_form,
        # 'a_form': ad_form
    }
    return render(request, 'users/profile.html', context)
