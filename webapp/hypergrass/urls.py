from django.urls import path
from .views import MissionListView, MissionDetailView
from . import views

app_name = 'hypergrass'

urlpatterns = [
    path('', views.home_view, name='home'),
    path('missions/', MissionListView.as_view(), name='missions'),
    # path('map-view/', views.map_view, name='map-viewer'),
    path('mission/<int:pk>/', MissionDetailView.as_view(), name='mission-detail'),

    # path('farms/', views.farms_view, name='farms'),
    # path('missions/', views.missions_view, name='missions'),
]