from django.db import models
from django.contrib.gis.db import models
from django.utils import timezone
from users.models import User
from django.urls import reverse

# class Farm(models.Model):
#     user = models.ForeignKey(User, on_delete=models.CASCADE)
#     name = models.CharFispectro-ag: notes.
#
# mineral valley project: asap
# germany project:  ~ month
# focus on simplicity of application:
# add dashboard,
# Open api for first test with hyperslit
#
# Corrections:
# connect S3 to app.
# andrej -> list of request from
#  hypersliteld(max_length=20)
#     crop = models.CharField(default='grass', max_length=20)
#     crop_variety = models.CharField(blank=True, max_length=20)
#     place_name = models.CharField(default='', max_length=30)
#     area_m2 = models.DecimalFiel
# Corrections:
# andrej -> list of request from hyperslitd(max_digits=7 ,decimal_places=2)
#     location = models.PointField(blank=True) # Open Gis compliant geometry
#
#     def __str__(self):
#         return self.name

class Mission(models.Model):
    owner = models.ForeignKey(User, on_delete=models.CASCADE)
    # farm = models.ForeignKey(Farm, on_delete=models.CASCADE)
    mission_name = models.CharField(max_length=50)
    date_mission = models.DateTimeField(default=timezone.now)
    raw_data_path = models.URLField(blank=True)
    results_path = models.URLField(blank=True)
    results_folder = models.CharField(blank=True, max_length=250)
    bbox_tl_x = models.DecimalField(null=True, max_digits= 10, decimal_places=5)
    bbox_tl_y = models.DecimalField(null=True, max_digits= 10, decimal_places=5)
    bbox_br_x = models.DecimalField(null=True, max_digits= 10, decimal_places=5)
    bbox_br_y = models.DecimalField(null=True, max_digits= 10, decimal_places=5)
    status_results = models.CharField(default='processing', max_length=20)

    def __str__(self):
        return self.mission_name

    def get_absolute_url(self):
        return reverse('hypergrass:mission-detail', kwargs={'pk': self.pk})
