# Long term ideas

- Fluent Validation  
  <http://fluentvalidation.codeplex.com/wikipage?title=Validators&referringTitle=Documentation>

- Introducing node-validation  
  <http://elegantcode.com/2013/05/03/introducing-node-validation/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ElegantCode+%28Elegant+Code%29>

- How to detect `Clean`, `Dirty`, `Invalid` status of `View` (not `ViewModel`)  
  <http://caliburnmicro.codeplex.com/discussions/434321>

- Attributes-based Validation in a WPF MVVM Application  
  <http://www.codeproject.com/Articles/97564/Attributes-based-Validation-in-a-WPF-MVVM-Applicat>

- EA Publish extensions  
  <http://caliburnmicro.codeplex.com/discussions/269685>

- Some ideas for method hooking on OSX

  -  Is it possible to hook API calls on Mac OS?  
    <http://stackoverflow.com/questions/2689348/is-it-possible-to-hook-api-calls-on-mac-os?rq=1>
  - Is hooking in iOS/Mac possible?  
    <http://stackoverflow.com/questions/7346323/is-hooking-in-ios-mac-possible>
  - MethodSwizzling  
    <http://cocoadev.com/wiki/MethodSwizzling>

- Remove `Loaded` (not working and not necessary at this time)

- `Parent` container should determine how its content will be aligned. Implement `ContentAlignment` dependency property.

- Check if scroll wheel support code works through composed views?

- Extend `Execute` (callbacks, exception handling, better async)

- Event binding syntax is currently:  
`[SetProperty('Click', '{Binding OnClick}')]`  
but it could be extended to resemble caliburn even more like this:  
`[MessageAttach('[Event Click] = [Action OnClick]')]`  
Notice: We do not support custom parameters on actions itd. like caliburn does! We can only bind methods with same signatures on `View`/`ViewModel`.

# Shorter term improvements

##### Bring sources in sync with the C# version of Caliburn Micro

- ConventionManager.cs PORTED Copied from DSharp. Compare with Caliburn.
- ElementConvention.cs PORTED Copied from DSharp. Compare with Caliburn.

The two units could be aligned to c# version more....when time permits.

##### When a property is bound, then automatically skip the binding of the Get/Set methods

See the `CalculatorViewModel` unit: it has Get/Set methods for every property so you can more easily trace into how the framework manages the bindings.

Even though the framework binds the properties, it also tries to bind the Get/Set methods.

##### The thread ping-pong trick.

       // Execute action on background thread, ensuring the main UI thread is initialized by the time focus is set
       Execute.OnBackgroundThread(
           // Main UI thread is initialized (Control is displayed on screen)           Execute.OnUIThread(

I'd rather have this phrased in code as something like "`QueueActionOnUIThread(...)`"

##### Errors when a binding cannot be established because of typing errors

For instance, there is no `Brush.Colr` property:

    bmTwoWay Binding Applied: property Color of @$02D33E20(ShellViewModel.TShellViewModel) to property Brush.Colr of Element Shape1(Vcl.ExtCtrls.TShape)
      of ShellView(ShellViewForm.TShellView)
      of @$00E2CE10(Vcl.Forms.TApplication).

##### do much smarter things with anonymous functions returning only 1 result

Try to see if it is possible to capture the name of the property from the IInterface that the Delphi compiler generates underneath:

    // if Delphi adds proper lamda expressions (versus anonymous methods) http://en.wikipedia.org/wiki/Lambda_(programming)
    // then we can get rid of the strings and do something like this:
    // NotifyOfPropertyChange(() => Count);
    NotifyOfPropertyChange('Count');

# Improved at

##### 20131108 - more generic logging in `DSharp.PresentationModel.VCLApplication`; demo in `program TasksVcl`:  

      Application.WithDebugLogger.Start<IShellViewModel>;
    //  Application.WithLogger<TCodeSiteLog>.Start<IShellViewModel>;
    
##### 20131107 - XE5 port

##### 20131106 - Dependencies on Spring for Delphi framework through `$(Spring)` in the search paths and `Dependencies*.*` files; XE5 packages are now run-time packages.
