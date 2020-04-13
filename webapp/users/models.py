from django.db import models
from django.contrib.auth.models import User
from PIL import Image
from phonenumber_field.modelfields import PhoneNumberField
# Create your models here.

class Profile(models.Model):
    user = models.OneToOneField(User, on_delete=models.CASCADE)
    image = models.ImageField(default='default.jpg', upload_to='profile_pics')
    #additional attributes go here
    first_name = models.CharField(blank=True, max_length=50)
    last_name = models.CharField(blank=True, max_length=50)
    phone = PhoneNumberField(null=True, unique=False)

    def __str__(self):
        return f'{self.user.username} Profile'

    # overwrite save method with extra functionality
    def save(self, *args, **kwargs):
        super(Profile, self).save(*args, **kwargs)

        img = Image.open(self.image.path)
        # resize images
        if img.height > 300 or img.width > 300:
            output_size = (300, 300)
            img.thumbnail(output_size)
            img.save(self.image.path)

class Address(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE) # many-to-one relationship
    address = models.CharField(max_length=30)
    postal_code = models.CharField(max_length=10)
    province = models.CharField(max_length=20)
    country = models.CharField(max_length=30)
