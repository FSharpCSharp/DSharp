------------------------------------------------------------------------------------------------------------------------------------------

Caliburn.Micro for Delphi 

is a small, yet powerful framework, designed for building applications across all major Delphi Platforms (XE,XE2,XE3). With strong support
for MVVM and other proven UI patterns, Caliburn.Micro will enable you to build your solution quickly, without the need to sacrifice code
quality or testability.

Brief list of features:

- Action Conventions
- Binding Conventions
- Screens and Conductors
- Event Aggregator
- Coroutines
- ViewLocator
- WindowManager
- PropertyChangedBase and BindableCollection
- Bootstrapper
- Logging
- MVVM and MVP

------------------------------------------------------------------------------------------------------------------------------------------

Current plan:

- Implement BuildUp. Look at SequentialResult. When iterating over a collection of IResult's each instance is passed through BuildUp to inject dependencies on the fly. MEF example:
	I suggest you implement this. We talked already but never came to a conclusion about it. Perhaps this is even not possible by current implementation of Spring DI? 
	If you create TSomeComponentWithDependency manually by calling Create. It should be possible to call IoC.BuildUp(SomeComponentWithDependency) and DI container would fill in the missing dependencies. For example some public property Log: ILog;

	protected override void BuildUp(object instance) {
		container.SatisfyImportsOnce(instance);
	}

- Finish the project DSharp.Presentation.Validation (example on how to use validation attributes)

- Implement additional interceptor classes for FMX controls (FMXControls.pas) and bring parity across VCL, FMX conventions (at least for simple ones...for example full support for grid on fmx would be quite time expensive)

- Implement AddChildValidatable

- Implement IValidatable

- Implement Async 

- Rename ValidationScreen to ValidatingViewModel

- Review API usage (especially the need to call validate methods from inside property setters when using data annotations,...)

- Add dummy System.Actions unit

WISHLIST

- Port SimpleContainer.cs

- Evaluate ConventionManager.cs to see if it is up to date

- IDE Expert

- Action messages

- Some basic work with the framework to discover rough edges and bugs