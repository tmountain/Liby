class Error
  @@debug = false

  def Error.error(status, fmt, *args)
    $stderr.printf("#{fmt}\n", *args)
    exit(status)
  end

  def Error.warn(fmt, *args)
    if @@debug
      $stderr.printf("#{fmt}\n", *args)
    end
  end

  def Error.activate_debug
    @@debug = true
  end

  def Error.deactivate_debug
    @@debug = false
  end

  def Error.debug
    @@debug
  end
end
