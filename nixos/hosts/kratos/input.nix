{ pkgs, ... }:
{
  # these have the same configuration surface
  services.libinput.mouse = { };
  services.libinput.touchpad = { };

  # accelPointsFallback = null;
  # accelPointsMotion = null;
  # accelPointsScroll = null;
  # accelProfile = "adaptive";
  # accelSpeed = null;
  # accelStepFallback = null;
  # accelStepMotion = null;
  # accelStepScroll = null;
  # additionalOptions = "";
  # buttonMapping = null;
  # calibrationMatrix = null;
  # clickMethod = null;
  # dev = null;
  # disableWhileTyping = false;
  # horizontalScrolling = true;
  # leftHanded = false;
  # middleEmulation = true;
  # naturalScrolling = false;
  # scrollButton = null;
  # scrollMethod = "twofinger";
  # sendEventsMode = "enabled";
  # tapping = true;
  # tappingButtonMap = null;
  # tappingDragLock = true;
  # transformationMatrix = null;
}
