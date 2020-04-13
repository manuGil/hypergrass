from django import forms
from django.contrib.auth.models import User
from django.contrib.auth.forms import UserCreationForm
from .models import Profile, Address

class UserRegisterForm(UserCreationForm):
    email = forms.EmailField() # email is required

    class Meta: # configurations
        model = User
        fields = ['username', 'email', 'password1', 'password2']

# Update username and email
class UserUpdateForm(forms.ModelForm):
    email = forms.EmailField() # email is required

    class Meta:
        model = User
        fields = ['username', 'email']

# update image profile
class ProfileUpdateForm(forms.ModelForm):
    class Meta:
        model = Profile
        fields = [ 'first_name', 'last_name', 'phone', 'image']

class AddressUpdateForm(forms.ModelForm):
        model= Address
        fields = ['address', 'postal_code', 'province', 'country']