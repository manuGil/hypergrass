from django.shortcuts import render
from .models import  Mission # Farm,
from django.views.generic import ListView, DetailView
import  json
from django.core.serializers.json import DjangoJSONEncoder
from django.contrib.auth.decorators import login_required
from django.contrib.auth.mixins import (
    LoginRequiredMixin, # restrict login
    UserPassesTestMixin)# restrict changes to missions by the owner
# Create your views here.


def home_view(request):
    return  render(request, 'hypergrass/home.html')

# @login_required
# def farms_view(request):
#     context = {
#         'farms':  Farm.objects.all() # Query to the database
#     }
#     return  render(request, 'hypergrass/farms.html', context)

def map_view(request):
    context = {
        'mission': Mission.objects.all() #TODO: filtr missions based on mission id, extract layer_forlder
    }
    return render(request, 'hypergrass/map-viewer.html')


# class-based views
class MissionListView(LoginRequiredMixin, ListView):
    model = Mission
    template_name = 'hypergrass/missions.html' # <app>/<model>_<view>.html
    context_object_name =  'missions'
    ordering = ['-date_mission']

class MissionDetailView(LoginRequiredMixin, UserPassesTestMixin, DetailView):
    model = Mission
    # folder = json.dumps(str(Mission.results_folder), cls=DjangoJSONEncoder)
    # template_name = 'hypergrass/mission_detail.html'

    def test_func(self):
        mission = self.get_object()
        if self.request.user == mission.owner:
            return True
        else:
            return False

#TODO: filter mission per user in view

# @login_required # doesn't work for classes
# def missions_view(request):
#     context = {
#         'missions': Mission.objects.all()
#     }
#     return render(request,'hypergrass/missions.html', context)
