from django.urls import path
from . import views as user_views

app_name = 'users'

urlpatterns = [
    path('', user_views.index, name='home'),
    path('signup/', user_views.singup_view, name='singup'),
    path('login/', user_views.login_view, name='login'),
    path('logout/', user_views.logout_view, name ='logout'),
    path('profile/', user_views.profile_view, name='profile'),
]
